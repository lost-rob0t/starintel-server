from __future__ import annotations

import re
from urllib.parse import urljoin, urlsplit

from bs4 import BeautifulSoup, Tag

from ..documents import document, evidence_record, relation, slugify, source_record
from ..models import BrowserPolicy, PageSnapshot, ScrapeBatch

_DATASET = "World Economic Forum"
_WEF = "World Economic Forum"
_YGL = "Forum of Young Global Leaders"
_CURRENT_SEED = "https://www.weforum.org/stories/leadership/meet-the-young-global-leaders-class-of-2026/"
_COHORT_RE = re.compile(r"Young Global Leaders Class of (?P<year>20\d{2})", re.I)
_MULTIPART_JURISDICTIONS = {("Hong Kong SAR", "People's Republic of China"), ("Macao SAR", "People's Republic of China")}
_PREFIXES = {"dr", "prof", "professor", "mr", "ms", "mrs", "minister", "baroness", "hon", "the", "h.e"}


def _text(tag: Tag | None) -> str:
    return " ".join(tag.stripped_strings) if tag else ""


def _clean(value: str) -> str:
    return re.sub(r"\s+", " ", value).strip(" \t\r\n.-")


def _name_parts(full_name: str) -> tuple[str, str, str]:
    tokens = [token for token in re.split(r"\s+", full_name.strip()) if token]
    while tokens and tokens[0].casefold().rstrip(".") in _PREFIXES:
        tokens.pop(0)
    if not tokens:
        return "", "", ""
    if len(tokens) == 1:
        return tokens[0], "", ""
    return tokens[0], " ".join(tokens[1:-1]), tokens[-1]


def _parse_story_line(raw: str) -> tuple[str, str, str]:
    parts = [_clean(part) for part in raw.split(",") if _clean(part)]
    if len(parts) < 2:
        return _clean(raw), "", ""
    name = parts[0]
    country_parts = [parts[-1]]
    role_end = -1
    if len(parts) >= 3 and (parts[-2], parts[-1]) in _MULTIPART_JURISDICTIONS:
        country_parts.insert(0, parts[-2])
        role_end = -2
    return name, ", ".join(parts[1:role_end]), ", ".join(country_parts)


class WefYoungGlobalLeadersAdapter:
    name = "wef-ygl"
    hosts = frozenset({"weforum.org", "www.weforum.org", "younggloballeaders.org", "www.younggloballeaders.org"})
    default_seeds = (_CURRENT_SEED,)
    browser_policy = BrowserPolicy.AUTO

    def matches(self, url: str) -> bool:
        host = (urlsplit(url).hostname or "").casefold()
        return host in self.hosts and ("young-global-leader" in url.casefold() or "younggloballeaders.org" in host)

    def parse(self, page: PageSnapshot) -> ScrapeBatch:
        soup = BeautifulSoup(page.html, "html.parser")
        source = source_record(page.final_url, fetched_at=page.fetched_at, content_hash=page.content_hash, access_method=page.access_method, response_status=page.status, publisher=_WEF)
        evidence = evidence_record(page.final_url, fetched_at=page.fetched_at, content_hash=page.content_hash)
        records, year = self._story_records(soup)
        if not records:
            records, year = self._legacy_records(soup)

        docs: list[dict[str, object]] = []
        wef_doc = document("org", {"name": _WEF, "display_name": _WEF, "org_type": "international-organization", "website": "https://www.weforum.org/", "external_ids": [{"scheme": "domain", "value": "weforum.org", "canonical": True}]}, dataset=_DATASET, natural_key="weforum.org", source=source, evidence=evidence, collected_at=page.fetched_at)
        ygl_doc = document("org", {"name": _YGL, "display_name": _YGL, "org_type": "leadership-community", "website": "https://www.younggloballeaders.org/", "parent_id": str(wef_doc["_id"]), "external_ids": [{"scheme": "domain", "value": "younggloballeaders.org", "canonical": True}]}, dataset=_DATASET, natural_key="younggloballeaders.org", source=source, evidence=evidence, collected_at=page.fetched_at)
        docs.extend((wef_doc, ygl_doc, relation(str(ygl_doc["_id"]), "part-of", str(wef_doc["_id"]), dataset=_DATASET, source=source, evidence=evidence, collected_at=page.fetched_at)))

        for record in records:
            full_name = record["name"]
            fname, mname, lname = _name_parts(full_name)
            cohort = record.get("year") or year
            normalized = " ".join(part for part in (fname, mname, lname) if part) or full_name
            key = f"{cohort}:{slugify(normalized)}"
            person = document("person", {"name": full_name, "display_name": full_name, "full_name": full_name, "fname": fname, "mname": mname, "lname": lname, "professional_affiliations": [_YGL, _WEF], "public_roles": [record["role"]] if record.get("role") else [], "country": record.get("country", ""), "misc": [value for value in (f"YGL cohort: {cohort}" if cohort else "", f"YGL category: {record['category']}" if record.get("category") else "", f"source line: {record['raw']}" if record.get("raw") else "") if value], "external_ids": [{"scheme": "wef:ygl:cohort-member", "value": key, "canonical": True, "url": page.final_url}]}, dataset=_DATASET, natural_key=key, source=source, evidence=evidence, collected_at=page.fetched_at)
            docs.extend((person, relation(str(person["_id"]), "member-of", str(ygl_doc["_id"]), dataset=_DATASET, source=source, evidence=evidence, collected_at=page.fetched_at, qualifiers={"cohort": cohort, "category": record.get("category", "")})))

        docs.append(document("source", source, dataset=_DATASET, natural_key=f"{page.final_url}|{page.content_hash}", source=source, evidence=evidence, collected_at=page.fetched_at))
        warnings = () if records else ("no Young Global Leaders records found; site structure may have changed",)
        return ScrapeBatch(self.name, page.final_url, tuple(docs), tuple(self._discover(soup, page.final_url)), warnings)

    def _story_records(self, soup: BeautifulSoup) -> tuple[list[dict[str, str]], str]:
        records: list[dict[str, str]] = []
        year = ""
        inside = False
        category = ""
        root = soup.find("article") or soup.find("main") or soup
        for tag in root.find_all(["h1", "h2", "h3", "li"]):
            if not isinstance(tag, Tag):
                continue
            text = _text(tag)
            if tag.name in {"h1", "h2"}:
                match = _COHORT_RE.search(text)
                if match:
                    inside = True
                    year = match.group("year")
                    continue
                if inside and tag.name == "h2":
                    break
                continue
            if not inside:
                continue
            if tag.name == "h3":
                category = _clean(text)
            elif tag.name == "li" and category:
                name, role, country = _parse_story_line(text)
                if name and len(name.split()) >= 2:
                    records.append({"name": name, "role": role, "country": country, "category": category, "year": year, "raw": text})
        return records, year

    def _legacy_records(self, soup: BeautifulSoup) -> tuple[list[dict[str, str]], str]:
        records: list[dict[str, str]] = []
        title_tag = soup.find("title")
        title = _text(title_tag if isinstance(title_tag, Tag) else None)
        match = re.search(r"20\d{2}", title)
        year = match.group(0) if match else ""
        for card in soup.select("article.person, .person-card, [data-testid*='member']"):
            name = _clean(_text(card.select_one("h3.person-name, h3, h2, [data-testid*='name']")))
            if not name:
                continue
            meta = _clean(_text(card.select_one("p.person-meta, .person-meta, [data-testid*='meta']")))
            _, role, country = _parse_story_line(f"{name}, {meta}" if meta else name)
            records.append({"name": name, "role": role, "country": country, "category": "community", "year": year, "raw": meta})
        return records, year

    def _discover(self, soup: BeautifulSoup, base_url: str) -> list[str]:
        out: list[str] = []
        seen: set[str] = set()
        for anchor in soup.find_all("a", href=True):
            url = urljoin(base_url, str(anchor.get("href")))
            if self.matches(url) and url not in seen and url != base_url and any(token in url.casefold() for token in ("/community", "/class", "/people", "/leader")):
                seen.add(url)
                out.append(url)
        return out

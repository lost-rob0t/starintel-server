from __future__ import annotations

import hashlib
import re
from typing import Iterable

SCHEMA_VERSION = "0.9.0"
_REQUIRED = {"_id", "dataset", "dtype", "schema_version", "version", "date_added", "date_updated", "sources", "evidence", "data"}


def stable_id(dtype: str, *parts: str) -> str:
    digest = hashlib.sha256("\x00".join((dtype, *parts)).encode()).hexdigest()[:40]
    return f"{dtype}:{digest}"


def slugify(value: str) -> str:
    slug = re.sub(r"[^a-z0-9]+", "-", value.casefold()).strip("-")
    return slug or hashlib.sha256(value.encode()).hexdigest()[:16]


def source_record(page_url: str, *, fetched_at: str, content_hash: str, access_method: str, response_status: int, publisher: str = "") -> dict[str, object]:
    return {
        "source_id": stable_id("source-ref", page_url, content_hash),
        "kind": "web-page",
        "publisher": publisher,
        "url": page_url,
        "retrieved_at": fetched_at,
        "access_method": access_method,
        "response_status": response_status,
        "content_hash": content_hash,
        "hash_algorithm": "sha256",
    }


def evidence_record(page_url: str, *, fetched_at: str, content_hash: str) -> dict[str, object]:
    return {
        "evidence_id": stable_id("evidence", page_url, content_hash),
        "source_url": page_url,
        "kind": "web-observation",
        "role": "primary",
        "collected_at": fetched_at,
        "content_hash": content_hash,
        "hash_algorithm": "sha256",
        "confidence": 1.0,
    }


def document(dtype: str, data: dict[str, object], *, dataset: str, natural_key: str, source: dict[str, object], evidence: dict[str, object], collected_at: str) -> dict[str, object]:
    return {
        "_id": stable_id(dtype, dataset, natural_key),
        "dataset": dataset,
        "dtype": dtype,
        "schema_version": SCHEMA_VERSION,
        "version": 1,
        "date_added": collected_at,
        "date_updated": collected_at,
        "sources": [source],
        "evidence": [evidence],
        "data": data,
    }


def relation(subject: str, predicate: str, object_: str, *, dataset: str, source: dict[str, object], evidence: dict[str, object], collected_at: str, qualifiers: dict[str, object] | None = None) -> dict[str, object]:
    data: dict[str, object] = {"subject": subject, "predicate": predicate, "object": object_, "source": subject, "target": object_, "directed": True}
    if qualifiers:
        data["qualifiers"] = qualifiers
    return document("relation", data, dataset=dataset, natural_key=f"{subject}|{predicate}|{object_}", source=source, evidence=evidence, collected_at=collected_at)


def validate_documents(values: Iterable[dict[str, object]]) -> None:
    for value in values:
        missing = _REQUIRED.difference(value)
        if missing:
            raise ValueError(f"missing canonical StarIntel fields: {sorted(missing)}")
        if value["schema_version"] != SCHEMA_VERSION:
            raise ValueError("unsupported schema_version")
        if not isinstance(value["version"], int) or value["version"] < 1:
            raise ValueError("version must be an integer >= 1")

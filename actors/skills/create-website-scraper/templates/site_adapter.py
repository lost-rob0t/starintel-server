from __future__ import annotations

from urllib.parse import urlsplit

from starintel_web_actor.models import BrowserPolicy, PageSnapshot, ScrapeBatch


class ExampleSiteAdapter:
    name = "example"
    hosts = frozenset({"example.com", "www.example.com"})
    default_seeds = ("https://example.com/",)
    browser_policy = BrowserPolicy.AUTO

    def matches(self, url: str) -> bool:
        return (urlsplit(url).hostname or "").casefold() in self.hosts

    def parse(self, page: PageSnapshot) -> ScrapeBatch:
        # Parse stable semantic HTML/JSON-LD and create canonical v0.9 documents.
        # Return discoveries to the runtime; do not recursively fetch here.
        return ScrapeBatch(site=self.name, url=page.final_url, documents=())

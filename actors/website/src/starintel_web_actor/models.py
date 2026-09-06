from __future__ import annotations

from dataclasses import dataclass
from enum import StrEnum
from typing import Protocol


class BrowserPolicy(StrEnum):
    NEVER = "never"
    AUTO = "auto"
    ALWAYS = "always"


@dataclass(frozen=True, slots=True)
class ScrapeRequest:
    url: str
    site: str | None = None
    browser: BrowserPolicy = BrowserPolicy.AUTO
    depth: int = 0
    wait_for: str | None = None


@dataclass(frozen=True, slots=True)
class PageSnapshot:
    requested_url: str
    final_url: str
    status: int
    headers: dict[str, str]
    html: str
    fetched_at: str
    access_method: str
    content_hash: str


@dataclass(frozen=True, slots=True)
class ScrapeBatch:
    site: str
    url: str
    documents: tuple[dict[str, object], ...]
    discovered_urls: tuple[str, ...] = ()
    warnings: tuple[str, ...] = ()


@dataclass(frozen=True, slots=True)
class CrawlReport:
    pages: int
    documents: int
    counts_by_dtype: dict[str, int]
    warnings: tuple[str, ...] = ()


class SiteAdapter(Protocol):
    name: str
    hosts: frozenset[str]
    default_seeds: tuple[str, ...]
    browser_policy: BrowserPolicy

    def matches(self, url: str) -> bool: ...
    def parse(self, page: PageSnapshot) -> ScrapeBatch: ...


class PageFetcher(Protocol):
    async def fetch(self, request: ScrapeRequest, *, adapter_policy: BrowserPolicy) -> PageSnapshot: ...
    async def close(self) -> None: ...


class DocumentSink(Protocol):
    async def write(self, documents: tuple[dict[str, object], ...]) -> None: ...
    async def close(self) -> None: ...

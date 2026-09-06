from __future__ import annotations

from .models import BrowserPolicy, CrawlReport, ScrapeRequest
from .registry import SiteRegistry
from .runtime import crawl_websites
from .sites.wef import WefYoungGlobalLeadersAdapter


def default_registry() -> SiteRegistry:
    registry = SiteRegistry()
    registry.register(WefYoungGlobalLeadersAdapter())
    registry.load_entrypoints()
    return registry


__all__ = ["BrowserPolicy", "CrawlReport", "ScrapeRequest", "SiteRegistry", "crawl_websites", "default_registry"]

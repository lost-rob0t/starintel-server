from __future__ import annotations

from collections import Counter
from dataclasses import dataclass

from .actor import Actor, ActorSystem
from .models import BrowserPolicy, CrawlReport, DocumentSink, PageFetcher, ScrapeRequest
from .registry import SiteRegistry


@dataclass(frozen=True, slots=True)
class CrawlJob:
    request: ScrapeRequest


@dataclass(slots=True)
class CrawlState:
    scheduled: int = 0
    pages: int = 0
    documents: int = 0
    counts: Counter[str] | None = None
    warnings: list[str] | None = None
    seen: set[str] | None = None

    def __post_init__(self) -> None:
        self.counts = Counter()
        self.warnings = []
        self.seen = set()


class WebsiteActor(Actor):
    def __init__(self, name: str, system: ActorSystem, *, registry: SiteRegistry, fetcher: PageFetcher, sink: DocumentSink, state: CrawlState, max_pages: int, max_depth: int) -> None:
        super().__init__(name, system, mailbox_size=max_pages)
        self.registry = registry
        self.fetcher = fetcher
        self.sink = sink
        self.state = state
        self.max_pages = max_pages
        self.max_depth = max_depth

    async def receive(self, message: object) -> None:
        if not isinstance(message, CrawlJob):
            raise TypeError(type(message).__name__)
        request = message.request
        adapter = self.registry.get(request.site) if request.site else self.registry.match(request.url)
        page = await self.fetcher.fetch(request, adapter_policy=adapter.browser_policy)
        batch = adapter.parse(page)
        await self.sink.write(batch.documents)
        self.state.pages += 1
        self.state.documents += len(batch.documents)
        self.state.warnings.extend(batch.warnings)
        for doc in batch.documents:
            self.state.counts[str(doc.get("dtype", "unknown"))] += 1
        if request.depth >= self.max_depth:
            return
        for url in batch.discovered_urls:
            if self.state.scheduled >= self.max_pages or url in self.state.seen:
                continue
            self.state.seen.add(url)
            self.state.scheduled += 1
            await self.ref.tell(CrawlJob(ScrapeRequest(url=url, site=adapter.name, browser=BrowserPolicy.AUTO, depth=request.depth + 1)))


async def crawl_websites(seeds: list[ScrapeRequest], *, registry: SiteRegistry, fetcher: PageFetcher, sink: DocumentSink, max_pages: int = 100, max_depth: int = 2) -> CrawlReport:
    state = CrawlState()
    system = ActorSystem()
    actor = WebsiteActor("website", system, registry=registry, fetcher=fetcher, sink=sink, state=state, max_pages=max_pages, max_depth=max_depth)
    system.register(actor)
    await system.start()
    try:
        for request in seeds:
            if request.url in state.seen or state.scheduled >= max_pages:
                continue
            state.seen.add(request.url)
            state.scheduled += 1
            await actor.ref.tell(CrawlJob(request))
        await actor.ref.join()
        if system.failures:
            raise RuntimeError(f"website actor failed: {system.failures[0].error}") from system.failures[0].error
    finally:
        await system.stop()
        await fetcher.close()
        await sink.close()
    return CrawlReport(state.pages, state.documents, dict(state.counts), tuple(state.warnings))

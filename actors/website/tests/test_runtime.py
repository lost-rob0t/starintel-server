from starintel_web_actor import ScrapeRequest, default_registry
from starintel_web_actor.models import PageSnapshot
from starintel_web_actor.runtime import crawl_websites


class Fetcher:
    async def fetch(self, request, *, adapter_policy):
        html = '<article><h2>Meet the Young Global Leaders Class of 2026</h2><h3>Business</h3><ul><li>Jane Doe, CEO, Example Inc, Canada.</li></ul><h2>End</h2></article>'
        return PageSnapshot(request.url, request.url, 200, {}, html, "2026-09-06T00:00:00+00:00", "http", "hash")

    async def close(self):
        pass


class Sink:
    def __init__(self):
        self.docs = []

    async def write(self, documents):
        self.docs.extend(documents)

    async def close(self):
        pass


async def test_actor_runtime_routes_fetch_parse_sink() -> None:
    sink = Sink()
    report = await crawl_websites([ScrapeRequest("https://www.weforum.org/stories/leadership/meet-the-young-global-leaders-class-of-2026/", site="wef-ygl")], registry=default_registry(), fetcher=Fetcher(), sink=sink, max_pages=1)
    assert report.pages == 1
    assert report.counts_by_dtype["person"] == 1
    assert sink.docs

from __future__ import annotations

import asyncio
from pathlib import Path
from typing import Annotated

import typer

from . import BrowserPolicy, ScrapeRequest, default_registry
from .browser import SmartFetcher
from .runtime import crawl_websites
from .sinks import JsonlSink
from .worker import serve

app = typer.Typer(no_args_is_help=True)


@app.command()
def sites() -> None:
    for name in default_registry().names():
        typer.echo(name)


@app.command()
def verify(site: str, browser: Annotated[BrowserPolicy, typer.Option()] = BrowserPolicy.AUTO, min_people: Annotated[int, typer.Option()] = 1) -> None:
    async def run() -> None:
        registry = default_registry()
        adapter = registry.get(site)
        fetcher = SmartFetcher()
        page = await fetcher.fetch(ScrapeRequest(adapter.default_seeds[0], site=site, browser=browser), adapter_policy=adapter.browser_policy)
        try:
            batch = adapter.parse(page)
        finally:
            await fetcher.close()
        people = sum(1 for doc in batch.documents if doc.get("dtype") == "person")
        typer.echo(f"site={site} status={page.status} method={page.access_method} people={people} documents={len(batch.documents)}")
        if people < min_people:
            raise typer.Exit(3)
    asyncio.run(run())


@app.command()
def crawl(site: str, url: str | None = None, output: Path = Path("/tmp/starintel-web.jsonl"), browser: BrowserPolicy = BrowserPolicy.AUTO) -> None:
    async def run() -> None:
        registry = default_registry()
        adapter = registry.get(site)
        report = await crawl_websites([ScrapeRequest(url or adapter.default_seeds[0], site=site, browser=browser)], registry=registry, fetcher=SmartFetcher(), sink=JsonlSink(output))
        typer.echo(f"pages={report.pages} documents={report.documents} output={output}")
    asyncio.run(run())


@app.command()
def health() -> None:
    async def run() -> None:
        import aio_pika
        from .worker import amqp_url_from_env
        connection = await aio_pika.connect_robust(amqp_url_from_env(), timeout=5)
        await connection.close()
        typer.echo("ok")
    asyncio.run(run())


@app.command()
def worker(site: str) -> None:
    asyncio.run(serve(site))


if __name__ == "__main__":
    app()

from __future__ import annotations

import json
import os
from pathlib import Path
from urllib.parse import quote

from . import BrowserPolicy, ScrapeRequest, default_registry
from .browser import SmartFetcher
from .runtime import crawl_websites
from .sinks import RabbitMqSink


def _secret(name: str, default: str = "") -> str:
    file_name = os.getenv(f"{name}_FILE")
    if file_name:
        return Path(file_name).read_text(encoding="utf-8").strip()
    return os.getenv(name, default)


def amqp_url_from_env() -> str:
    host = os.getenv("RABBITMQ_HOST", "rabbitmq")
    port = int(os.getenv("RABBITMQ_PORT", "5672"))
    user = os.getenv("RABBITMQ_USER", "starintel")
    password = _secret("RABBITMQ_PASSWORD")
    return f"amqp://{quote(user, safe='')}:{quote(password, safe='')}@{host}:{port}/"


def target_value(payload: dict[str, object]) -> str:
    data = payload.get("data")
    if isinstance(data, dict) and isinstance(data.get("target"), str):
        return data["target"]
    value = payload.get("target")
    if isinstance(value, str):
        return value
    raise ValueError("target payload does not contain a string target")


async def serve(site: str) -> None:
    import aio_pika

    registry = default_registry()
    adapter = registry.get(site)
    actor_name = os.getenv("STARINTEL_ACTOR_NAME", site)
    amqp_url = amqp_url_from_env()
    connection = await aio_pika.connect_robust(amqp_url)
    channel = await connection.channel()
    await channel.set_qos(prefetch_count=int(os.getenv("STARINTEL_PREFETCH", "1")))
    exchange = await channel.declare_exchange("documents", aio_pika.ExchangeType.TOPIC, durable=True)
    queue = await channel.declare_queue(f"starintel.actor.{actor_name}", durable=True)
    await queue.bind(exchange, routing_key=f"documents.target.dispatch.{actor_name}")
    await queue.bind(exchange, routing_key=f"actors.{actor_name}.new.target")

    async with queue.iterator() as messages:
        async for message in messages:
            async with message.process(requeue=True):
                payload = json.loads(message.body)
                target = target_value(payload)
                fetcher = SmartFetcher(storage_state=Path(os.environ["PLAYWRIGHT_STORAGE_STATE_FILE"]) if os.getenv("PLAYWRIGHT_STORAGE_STATE_FILE") else None)
                sink = RabbitMqSink(amqp_url)
                await crawl_websites([ScrapeRequest(target, site=adapter.name, browser=BrowserPolicy.AUTO)], registry=registry, fetcher=fetcher, sink=sink, max_pages=int(os.getenv("STARINTEL_MAX_PAGES", "100")), max_depth=int(os.getenv("STARINTEL_MAX_DEPTH", "2")))

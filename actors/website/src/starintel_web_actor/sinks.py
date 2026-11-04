from __future__ import annotations

import json
from pathlib import Path

from .documents import validate_documents


class JsonlSink:
    def __init__(self, path: Path) -> None:
        self.path = path
        self.path.parent.mkdir(parents=True, exist_ok=True)

    async def write(self, documents: tuple[dict[str, object], ...]) -> None:
        validate_documents(documents)
        with self.path.open("a", encoding="utf-8") as handle:
            for document in documents:
                handle.write(json.dumps(document, ensure_ascii=False, sort_keys=True) + "\n")

    async def close(self) -> None:
        return


class RabbitMqSink:
    def __init__(self, amqp_url: str, *, exchange: str = "documents") -> None:
        self.amqp_url = amqp_url
        self.exchange_name = exchange
        self._connection = None
        self._channel = None
        self._exchange = None

    async def _ensure(self) -> None:
        if self._exchange is not None:
            return
        import aio_pika
        self._connection = await aio_pika.connect_robust(self.amqp_url)
        self._channel = await self._connection.channel(publisher_confirms=True)
        self._exchange = await self._channel.declare_exchange(self.exchange_name, aio_pika.ExchangeType.TOPIC, durable=True)

    async def write(self, documents: tuple[dict[str, object], ...]) -> None:
        import aio_pika
        validate_documents(documents)
        await self._ensure()
        for document in documents:
            body = json.dumps(document, ensure_ascii=False, separators=(",", ":")).encode()
            message = aio_pika.Message(body=body, content_type="application/json", type=str(document["dtype"]), delivery_mode=aio_pika.DeliveryMode.PERSISTENT)
            await self._exchange.publish(message, routing_key=f"documents.ingest.{document['dtype']}")

    async def close(self) -> None:
        if self._connection is not None:
            await self._connection.close()

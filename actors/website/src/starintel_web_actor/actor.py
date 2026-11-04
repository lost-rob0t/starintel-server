from __future__ import annotations

import asyncio
import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass

LOGGER = logging.getLogger(__name__)


@dataclass(frozen=True, slots=True)
class Stop:
    pass


@dataclass(frozen=True, slots=True)
class ActorFailure:
    actor: str
    error: BaseException


class ActorRef:
    def __init__(self, name: str, mailbox: asyncio.Queue[object]) -> None:
        self.name = name
        self._mailbox = mailbox

    async def tell(self, message: object) -> None:
        await self._mailbox.put(message)

    async def join(self) -> None:
        await self._mailbox.join()


class Actor(ABC):
    def __init__(self, name: str, system: ActorSystem, mailbox_size: int = 1000) -> None:
        self.name = name
        self.system = system
        self.mailbox: asyncio.Queue[object] = asyncio.Queue(maxsize=mailbox_size)
        self.ref = ActorRef(name, self.mailbox)
        self._task: asyncio.Task[None] | None = None

    async def start(self) -> None:
        if self._task is not None:
            raise RuntimeError(f"actor already started: {self.name}")
        self._task = asyncio.create_task(self._run(), name=f"actor:{self.name}")

    async def stop(self) -> None:
        if self._task is None:
            return
        await self.ref.tell(Stop())
        await self._task
        self._task = None

    async def _run(self) -> None:
        while True:
            message = await self.mailbox.get()
            try:
                if isinstance(message, Stop):
                    return
                await self.receive(message)
            except asyncio.CancelledError:
                raise
            except Exception as exc:
                LOGGER.exception("actor %s failed handling %s", self.name, type(message).__name__)
                self.system.failures.append(ActorFailure(self.name, exc))
            finally:
                self.mailbox.task_done()

    @abstractmethod
    async def receive(self, message: object) -> None:
        raise NotImplementedError


class ActorSystem:
    def __init__(self) -> None:
        self._actors: dict[str, Actor] = {}
        self.failures: list[ActorFailure] = []

    def register(self, actor: Actor) -> ActorRef:
        if actor.name in self._actors:
            raise ValueError(f"duplicate actor name: {actor.name}")
        self._actors[actor.name] = actor
        return actor.ref

    async def start(self) -> None:
        for actor in self._actors.values():
            await actor.start()

    async def stop(self) -> None:
        for actor in reversed(tuple(self._actors.values())):
            await actor.stop()

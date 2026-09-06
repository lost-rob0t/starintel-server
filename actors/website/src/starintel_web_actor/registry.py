from __future__ import annotations

from importlib.metadata import entry_points

from .models import SiteAdapter


class SiteRegistry:
    def __init__(self) -> None:
        self._sites: dict[str, SiteAdapter] = {}

    def register(self, adapter: SiteAdapter) -> None:
        if adapter.name in self._sites:
            raise ValueError(f"duplicate site adapter: {adapter.name}")
        self._sites[adapter.name] = adapter

    def get(self, name: str) -> SiteAdapter:
        try:
            return self._sites[name]
        except KeyError as exc:
            raise KeyError(f"unknown site adapter: {name}") from exc

    def match(self, url: str) -> SiteAdapter:
        matches = [site for site in self._sites.values() if site.matches(url)]
        if len(matches) != 1:
            raise ValueError(f"expected exactly one site adapter for {url!r}, got {len(matches)}")
        return matches[0]

    def load_entrypoints(self) -> None:
        for ep in entry_points(group="starintel.website_actors"):
            factory = ep.load()
            self.register(factory())

    def names(self) -> tuple[str, ...]:
        return tuple(sorted(self._sites))

from __future__ import annotations

import hashlib
import re
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

import httpx

from .models import BrowserPolicy, PageSnapshot, ScrapeRequest

_JS_SHELL = re.compile(r"enable javascript|javascript is required|__next_data__|id=[\"'](?:root|app)[\"']", re.I)
_BLOCKED = {401, 403, 429}


def _snapshot(requested_url: str, final_url: str, status: int, headers: dict[str, str], html: str, method: str) -> PageSnapshot:
    return PageSnapshot(requested_url, final_url, status, headers, html, datetime.now(UTC).isoformat(), method, hashlib.sha256(html.encode("utf-8", errors="replace")).hexdigest())


def needs_browser(page: PageSnapshot) -> bool:
    if page.status in _BLOCKED:
        return True
    text = re.sub(r"\s+", " ", re.sub(r"<[^>]+>", " ", page.html)).strip()
    return len(text) < 800 and _JS_SHELL.search(page.html) is not None


class SmartFetcher:
    def __init__(self, *, timeout: float = 30.0, user_agent: str = "StarIntel-Web-Actor/0.1 (+https://starintel.actor)", storage_state: Path | None = None) -> None:
        self.timeout = timeout
        self.user_agent = user_agent
        self.storage_state = storage_state
        self._client = httpx.AsyncClient(timeout=httpx.Timeout(timeout), follow_redirects=True, http2=True, headers={"User-Agent": user_agent, "Accept": "text/html,application/xhtml+xml;q=0.9,*/*;q=0.1"})
        self._pw: Any = None
        self._browser: Any = None

    async def fetch(self, request: ScrapeRequest, *, adapter_policy: BrowserPolicy = BrowserPolicy.AUTO) -> PageSnapshot:
        policy = request.browser if request.browser != BrowserPolicy.AUTO else adapter_policy
        if policy != BrowserPolicy.ALWAYS:
            try:
                response = await self._client.get(request.url)
                page = _snapshot(request.url, str(response.url), response.status_code, dict(response.headers), response.text, "http")
                if policy == BrowserPolicy.NEVER or not needs_browser(page):
                    return page
            except httpx.HTTPError:
                if policy == BrowserPolicy.NEVER:
                    raise
        return await self._fetch_browser(request)

    async def _ensure_browser(self) -> None:
        if self._browser is not None:
            return
        from playwright.async_api import async_playwright
        self._pw = await async_playwright().start()
        self._browser = await self._pw.chromium.launch(headless=True)

    async def _fetch_browser(self, request: ScrapeRequest) -> PageSnapshot:
        await self._ensure_browser()
        context = await self._browser.new_context(user_agent=self.user_agent, storage_state=str(self.storage_state) if self.storage_state else None)
        page = await context.new_page()
        await page.route("**/*", lambda route: route.abort() if route.request.resource_type in {"image", "media", "font"} else route.continue_())
        response = await page.goto(request.url, wait_until="domcontentloaded", timeout=int(self.timeout * 1000))
        if request.wait_for:
            await page.locator(request.wait_for).first.wait_for(timeout=int(self.timeout * 1000))
        try:
            await page.wait_for_load_state("networkidle", timeout=5000)
        except Exception:
            pass
        html = await page.content()
        final_url = page.url
        status = response.status if response else 200
        headers = await response.all_headers() if response else {}
        await context.close()
        return _snapshot(request.url, final_url, status, headers, html, "playwright-chromium")

    async def close(self) -> None:
        await self._client.aclose()
        if self._browser is not None:
            await self._browser.close()
        if self._pw is not None:
            await self._pw.stop()

from starintel_web_actor.browser import needs_browser
from starintel_web_actor.models import PageSnapshot


def snap(status: int, html: str) -> PageSnapshot:
    return PageSnapshot("https://example.com", "https://example.com", status, {}, html, "2026-09-06T00:00:00+00:00", "http", "hash")


def test_block_and_js_shell_trigger_browser() -> None:
    assert needs_browser(snap(403, "blocked"))
    assert needs_browser(snap(200, '<div id="root"></div><script>app()</script>'))
    assert not needs_browser(snap(200, "<p>" + ("content " * 200) + "</p>"))

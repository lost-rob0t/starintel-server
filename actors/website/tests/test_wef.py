from pathlib import Path

from starintel_web_actor.models import PageSnapshot
from starintel_web_actor.sites.wef import WefYoungGlobalLeadersAdapter


def page() -> PageSnapshot:
    html = (Path(__file__).parent / "fixtures" / "wef_ygl_2026.html").read_text()
    return PageSnapshot("https://www.weforum.org/stories/leadership/meet-the-young-global-leaders-class-of-2026/", "https://www.weforum.org/stories/leadership/meet-the-young-global-leaders-class-of-2026/", 200, {}, html, "2026-09-06T12:00:00+00:00", "http", "abc123")


def test_current_wef_layout_emits_canonical_people() -> None:
    batch = WefYoungGlobalLeadersAdapter().parse(page())
    people = [d for d in batch.documents if d["dtype"] == "person"]
    assert len(people) == 4
    names = {d["data"]["full_name"] for d in people}
    assert "Dr Paulo Savaget" in names
    arthur = next(d for d in people if d["data"]["full_name"] == "Arthur Law")
    assert arthur["data"]["country"] == "Hong Kong SAR, People's Republic of China"
    assert all(d["schema_version"] == "0.9.0" and d["version"] == 1 for d in batch.documents)
    assert "email" not in repr(batch.documents).lower()


def test_wef_ids_are_deterministic() -> None:
    adapter = WefYoungGlobalLeadersAdapter()
    assert [d["_id"] for d in adapter.parse(page()).documents] == [d["_id"] for d in adapter.parse(page()).documents]

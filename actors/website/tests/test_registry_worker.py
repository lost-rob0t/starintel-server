import pytest

from starintel_web_actor import default_registry
from starintel_web_actor.worker import target_value


def test_registry_routes_wef() -> None:
    adapter = default_registry().match("https://www.weforum.org/stories/leadership/meet-the-young-global-leaders-class-of-2026/")
    assert adapter.name == "wef-ygl"


def test_target_value_supports_current_and_legacy_envelopes() -> None:
    assert target_value({"data": {"target": "https://example.com"}}) == "https://example.com"
    assert target_value({"target": "https://example.net"}) == "https://example.net"
    with pytest.raises(ValueError):
        target_value({"data": {}})

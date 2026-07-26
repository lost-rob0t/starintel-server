from __future__ import annotations

import json
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
VIEWS = ROOT / "source" / "views"

EXPECTED = {
    "messages-by-user": ("messages", "messages_by_user"),
    "messages-by-platform": ("messages", "messages_by_platform"),
    "messages-by-group": ("messages", "messages_by_group"),
    "social-posts-by-user": ("messages", "social_posts_by_user"),
    "social-posts-by-group": ("messages", "social_posts_by_group"),
    "social-posts-by-platform": ("messages", "social_posts_by_platform"),
    "by-channel": ("messages", "by_channel"),
    "groups": ("messages", "groups"),
    "count-by-dtype": ("data", "count_by_dtype"),
    "dataset-size": ("data", "dataset_size"),
    "documents-by-dataset": ("data", "by_dataset"),
    "orgs-by-country": ("orgs", "by_country"),
    "orgs-by-name": ("orgs", "by_name"),
    "persons-by-name": ("persons", "by_name"),
    "persons-by-region": ("persons", "by_region"),
    "relations-edges": ("relations", "edges"),
    "relations-incoming-count": ("relations", "incoming_count"),
    "relations-outgoing-count": ("relations", "outgoing_count"),
    "targets-actor-counts": ("targets", "actor_count"),
    "targets-by-actor": ("targets", "by_actor"),
    "targets-target-count": ("targets", "target_count"),
    "users-by-platform": ("users", "by_platform"),
    "timeline-view": ("time", "timeline"),
}


class ViewRegistryContractTests(unittest.TestCase):
    def setUp(self) -> None:
        self.registry = (
            ROOT / "source" / "databases" / "view-registry.lisp"
        ).read_text(encoding="utf-8")
        self.designs = {
            path.stem: json.loads(path.read_text(encoding="utf-8"))
            for path in VIEWS.glob("*.json")
        }

    def test_every_registered_wrapper_targets_checked_in_view(self) -> None:
        for wrapper, (design, view) in EXPECTED.items():
            pattern = re.compile(
                rf"register-view-spec\s+'{re.escape(wrapper)}\s+"
                rf'"{re.escape(design)}"\s+"{re.escape(view)}"',
                re.DOTALL,
            )
            self.assertRegex(self.registry, pattern, wrapper)
            self.assertIn(design, self.designs, wrapper)
            self.assertEqual(self.designs[design]["_id"], f"_design/{design}")
            self.assertIn(view, self.designs[design].get("views", {}), wrapper)

    def test_target_canonical_names_and_legacy_aliases_coexist(self) -> None:
        views = self.designs["targets"]["views"]
        for name in ("by_actor", "actor_count", "target_count"):
            self.assertIn(name, views)
        for alias in ("actor-targets", "actor-target-count", "target-count"):
            self.assertIn(alias, views)

    def test_data_and_users_missing_views_are_installed(self) -> None:
        self.assertIn("by_dataset", self.designs["data"]["views"])
        self.assertIn("by_platform", self.designs["users"]["views"])
        self.assertIn("doc.dtype === 'user'", self.designs["users"]["views"]["by_platform"]["map"])

    def test_social_post_views_use_one_canonical_dtype(self) -> None:
        views = self.designs["messages"]["views"]
        for name in (
            "social_posts_by_user",
            "social_posts_by_group",
            "social_posts_by_platform",
        ):
            source = views[name]["map"]
            self.assertIn("social-media-post", source)
            self.assertNotIn("social-media-posts", source)

    def test_registry_validates_keywords_and_result_shapes(self) -> None:
        for token in (
            ":group",
            ":group-level",
            "reduced view requests cannot include documents",
            "group/group-level requires reduce=true",
            "has no reducer",
            "view-document-result",
            "view-map-result",
            "view-reduced-result",
        ):
            self.assertIn(token, self.registry)
        self.assertIn("define-registered-view-wrapper by-channel", self.registry)

    def test_startup_validation_precedes_view_install_and_traffic(self) -> None:
        source = (ROOT / "source" / "init.lisp").read_text(encoding="utf-8")
        validation = source.index("validate-view-registry")
        installation = source.rindex("(init-views client database)")
        self.assertLess(validation, installation)

    def test_http_routes_use_registry_and_keep_explicit_aliases(self) -> None:
        source = (
            ROOT / "source" / "frontends" / "http-view-registry.lisp"
        ).read_text(encoding="utf-8")
        for route in (
            "/documents/messages/by-user",
            "/documents/messages/by-platform",
            "/documents/messages/by-channel",
            "/documents/messages/by-groups",
            "/documents/messages/groups",
            "/documents/social-media-posts/by-user",
            "/documents/social-media-posts/by-group",
            "/documents/social-media-posts/by-platform",
            "/documents/socialmpost/by-user",
            "/dataset-size",
            "/targets/:actor",
        ):
            self.assertIn(route, source)
        self.assertIn("execute-registered-view", source)
        self.assertIn("*http-view-registry-matrix*", source)

    def test_executable_acceptance_matrix_is_in_docker_gate(self) -> None:
        tests = (
            ROOT / "tests" / "run-view-registry-conformance.lisp"
        ).read_text(encoding="utf-8")
        dockerfile = (ROOT / "Dockerfile").read_text(encoding="utf-8")
        for name in (
            "test-checked-in-design-documents-satisfy_registry",
            "test-missing-expected-view_fails_validation",
            "test_typed_document_map_and_reduced_results",
            "test_by_channel_accepts_map_and_reduced_modes",
            "test_impossible_result_shapes_fail_before_query",
            "test_social_post_views_use_canonical_dtype",
        ):
            self.assertIn(name, tests)
        self.assertIn("run-view-registry-conformance.lisp", dockerfile)


if __name__ == "__main__":
    unittest.main()

% Regression fixtures for the StarIntel migration expert system.
:- begin_tests(starintel_migrations).

:- use_module('migrations.pl').

legacy_fixture(Doc) :-
    Doc = _{
        '_id':"fixture-legacy-1",
        '_rev':"3-fixture",
        tenant_id:"tenant-a",
        dataset:"dataset-a",
        dtype:"person",
        schema_version:"0.8.0",
        version:7,
        date_added:"2026-01-01T00:00:00Z",
        date_updated:"2026-01-02T00:00:00Z",
        name:"Alice Example"
    }.

test(release_is_not_document_schema) :-
    current_schema_version("0.9.0"),
    current_release_version("0.9.1"),
    \+ migration_edge("0.9.0", "0.9.1").

test(historical_path_reaches_current) :-
    migration_path("0.7.3", "0.9.0",
                   ["0.7.3", "0.8.0", "0.9.0"]).

test(promote_current_emits_scoped_full_document) :-
    legacy_fixture(Doc),
    map_view(promote_current, Doc,
             [[["tenant-a", "dataset-a"], Migrated]]),
    get_dict('_id', Migrated, "fixture-legacy-1"),
    get_dict('_rev', Migrated, "3-fixture"),
    get_dict(schema_version, Migrated, "0.9.0"),
    get_dict(version, Migrated, 7),
    get_dict(data, Migrated, Data),
    get_dict(name, Data, "Alice Example"),
    get_dict(lineage, Migrated, Lineage),
    get_dict(migration_from, Lineage, "0.8.0").

test(current_document_has_no_migration_candidate) :-
    legacy_fixture(Legacy),
    put_dict(schema_version, Legacy, "0.9.0", Current),
    map_view(promote_current, Current, []).

test(unknown_schema_has_no_migration_candidate) :-
    legacy_fixture(Legacy),
    put_dict(schema_version, Legacy, "99.99.99", Unknown),
    map_view(promote_current, Unknown, []).

test(version_distribution_is_tenant_dataset_scoped) :-
    legacy_fixture(Doc),
    map_view(version_distribution, Doc,
             [[["tenant-a", "dataset-a", "0.8.0", "person"], 1]]).

:- end_tests(starintel_migrations).

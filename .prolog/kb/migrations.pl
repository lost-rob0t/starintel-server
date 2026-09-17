% StarIntel schema migration expert system.
% CouchDB's query server asks this KB to classify and project migration rows.
% The KB never evaluates caller supplied Prolog.  Only map_view/3 is public.

:- module(starintel_migrations,
          [ current_schema_version/1,
            current_release_version/1,
            migration_edge/2,
            migration_path/3,
            detected_schema/2,
            migrate_to_current/3,
            map_view/3
          ]).

:- use_module(library(lists)).

current_schema_version("0.9.0").
current_release_version("0.9.1").

% Historical compatibility graph.  The release/profile version is deliberately
% absent: 0.9.1 is a release over the immutable 0.9.0 document schema.
migration_edge("legacy-booker", "0.7.3").
migration_edge("legacy-pre-0.7.3", "0.7.3").
migration_edge("0.4.5", "0.4.6").
migration_edge("0.4.6", "0.5.0").
migration_edge("0.5.0", "0.5.1").
migration_edge("0.5.1", "0.5.2").
migration_edge("0.5.2", "0.5.3").
migration_edge("0.5.3", "0.5.4").
migration_edge("0.5.4", "0.6.0").
migration_edge("0.6.0", "0.7.0").
migration_edge("0.7.0", "0.7.3").
migration_edge("0.7.3", "0.8.0").
migration_edge("0.8.0", "0.9.0").

migration_path(From, To, Path) :-
    migration_path_(From, To, [From], Reversed),
    reverse(Reversed, Path).

migration_path_(To, To, Seen, Seen) :- !.
migration_path_(From, To, Seen, Path) :-
    migration_edge(From, Next),
    \+ memberchk(Next, Seen),
    migration_path_(Next, To, [Next|Seen], Path).

design_document(Doc) :-
    get_dict('_id', Doc, Id),
    string(Id),
    sub_string(Id, 0, 8, _, "_design/").

nonempty_string(Value) :-
    string(Value),
    string_length(Value, Length),
    Length > 0.

dict_string(Dict, Key, Value) :-
    get_dict(Key, Dict, Value),
    nonempty_string(Value).

known_historical_version("0.4.5").
known_historical_version("0.4.6").
known_historical_version("0.5.0").
known_historical_version("0.5.1").
known_historical_version("0.5.2").
known_historical_version("0.5.3").
known_historical_version("0.5.4").
known_historical_version("0.6.0").
known_historical_version("0.7.0").
known_historical_version("0.7.3").
known_historical_version("0.8.0").
known_historical_version("0.9.0").

detected_schema(Doc, Version) :-
    is_dict(Doc),
    dict_string(Doc, schema_version, Version),
    !.
detected_schema(Doc, Version) :-
    is_dict(Doc),
    dict_string(Doc, version, Version),
    known_historical_version(Version),
    !.
detected_schema(Doc, "legacy-booker") :-
    is_dict(Doc),
    get_dict(type, Doc, Type),
    nonempty_string(Type),
    ( get_dict(metadata, Doc, _)
    ; get_dict(private_metadata, Doc, _)
    ; get_dict(source_dataset, Doc, _)
    ),
    !.
detected_schema(Doc, "legacy-pre-0.7.3") :-
    is_dict(Doc),
    dict_string(Doc, dtype, _),
    \+ get_dict(schema_version, Doc, _),
    ( dict_string(Doc, dataset, _)
    ; dict_string(Doc, source_dataset, _)
    ),
    !.
detected_schema(_, "unknown").

document_id(Doc, Id) :-
    dict_string(Doc, '_id', Id).

document_dataset(Doc, Dataset) :-
    dict_string(Doc, dataset, Dataset),
    !.
document_dataset(Doc, Dataset) :-
    dict_string(Doc, source_dataset, Dataset),
    !.
document_dataset(Doc, Dataset) :-
    dict_string(Doc, sourceDataset, Dataset).

document_tenant(Doc, Tenant) :-
    dict_string(Doc, tenant_id, Tenant),
    !.
document_tenant(Doc, Tenant) :-
    dict_string(Doc, tenant, Tenant),
    !.
document_tenant(_, "default").

dtype_alias("organization", "org").
dtype_alias("organisation", "org").
dtype_alias("investigation_target", "investigation-target").

known_dtype("actor-manifest").
known_dtype("address").
known_dtype("alert").
known_dtype("analysis").
known_dtype("asset").
known_dtype("breach").
known_dtype("campaign-finance").
known_dtype("claim").
known_dtype("concept").
known_dtype("contract").
known_dtype("dataset-manifest").
known_dtype("document").
known_dtype("domain").
known_dtype("education").
known_dtype("email").
known_dtype("email-message").
known_dtype("employment").
known_dtype("entity").
known_dtype("event").
known_dtype("evidence-record").
known_dtype("file").
known_dtype("financial-observation").
known_dtype("geo").
known_dtype("grant").
known_dtype("host").
known_dtype("investigation-target").
known_dtype("legal-case").
known_dtype("lobbying-filing").
known_dtype("location").
known_dtype("media").
known_dtype("meeting").
known_dtype("message").
known_dtype("network").
known_dtype("observation").
known_dtype("operation").
known_dtype("org").
known_dtype("ownership").
known_dtype("person").
known_dtype("phone").
known_dtype("policy").
known_dtype("procurement").
known_dtype("product").
known_dtype("relation").
known_dtype("research-node").
known_dtype("research-pass").
known_dtype("social-media-post").
known_dtype("source").
known_dtype("target").
known_dtype("task").
known_dtype("url").
known_dtype("user").

canonical_dtype(Raw, Dtype) :-
    dtype_alias(Raw, Alias),
    !,
    Dtype = Alias.
canonical_dtype(Raw, Raw) :-
    known_dtype(Raw),
    !.
canonical_dtype(_, "document").

document_dtype(Doc, Dtype) :-
    ( dict_string(Doc, dtype, Raw)
    ; dict_string(Doc, type, Raw)
    ),
    !,
    canonical_dtype(Raw, Dtype).
document_dtype(_, "document").

schema_org_type("actor-manifest", "CreativeWork").
schema_org_type("address", "PostalAddress").
schema_org_type("alert", "SpecialAnnouncement").
schema_org_type("analysis", "CreativeWork").
schema_org_type("asset", "Thing").
schema_org_type("breach", "Event").
schema_org_type("campaign-finance", "CreativeWork").
schema_org_type("claim", "Claim").
schema_org_type("concept", "DefinedTerm").
schema_org_type("contract", "DigitalDocument").
schema_org_type("dataset-manifest", "Dataset").
schema_org_type("document", "CreativeWork").
schema_org_type("domain", "WebSite").
schema_org_type("education", "EducationalOccupationalCredential").
schema_org_type("email", "ContactPoint").
schema_org_type("email-message", "Message").
schema_org_type("employment", "OrganizationRole").
schema_org_type("entity", "Thing").
schema_org_type("event", "Event").
schema_org_type("evidence-record", "CreativeWork").
schema_org_type("file", "DigitalDocument").
schema_org_type("financial-observation", "CreativeWork").
schema_org_type("geo", "GeoCoordinates").
schema_org_type("grant", "Grant").
schema_org_type("host", "Thing").
schema_org_type("investigation-target", "Thing").
schema_org_type("legal-case", "CreativeWork").
schema_org_type("lobbying-filing", "DigitalDocument").
schema_org_type("location", "Place").
schema_org_type("media", "MediaObject").
schema_org_type("meeting", "Event").
schema_org_type("message", "Message").
schema_org_type("network", "Thing").
schema_org_type("observation", "CreativeWork").
schema_org_type("operation", "Action").
schema_org_type("org", "Organization").
schema_org_type("ownership", "Role").
schema_org_type("person", "Person").
schema_org_type("phone", "ContactPoint").
schema_org_type("policy", "CreativeWork").
schema_org_type("procurement", "DigitalDocument").
schema_org_type("product", "Product").
schema_org_type("relation", "Role").
schema_org_type("research-node", "Action").
schema_org_type("research-pass", "CreativeWork").
schema_org_type("social-media-post", "SocialMediaPosting").
schema_org_type("source", "CreativeWork").
schema_org_type("target", "Thing").
schema_org_type("task", "Action").
schema_org_type("url", "WebPage").
schema_org_type("user", "Person").

schema_org_metadata(Dtype, Id, SchemaOrg) :-
    ( schema_org_type(Dtype, Type) -> true ; Type = "Thing" ),
    string_concat("https://starintel.dev/dtype/", Dtype, AdditionalType),
    SchemaOrg = _{
        '@context':"https://schema.org/",
        '@type':Type,
        '@id':Id,
        additionalType:AdditionalType
    }.

date_value(Doc, Canonical, _, Value) :-
    dict_string(Doc, Canonical, Value),
    !.
date_value(Doc, _, Alias, Value) :-
    dict_string(Doc, Alias, Value).

document_dates(Doc, Added, Updated) :-
    ( date_value(Doc, date_added, dateAdded, Added0) -> true ; Added0 = "" ),
    ( date_value(Doc, date_updated, dateUpdated, Updated0) -> true ; Updated0 = "" ),
    ( nonempty_string(Added0) -> Added = Added0
    ; nonempty_string(Updated0) -> Added = Updated0
    ),
    ( nonempty_string(Updated0) -> Updated = Updated0
    ; Updated = Added
    ).

canonical_version(Doc, Version) :-
    get_dict(version, Doc, Raw),
    integer(Raw),
    Raw >= 1,
    !,
    Version = Raw.
canonical_version(_, 1).

common_key('_id').
common_key('_rev').
common_key(dataset).
common_key(dtype).
common_key(schema_version).
common_key(version).
common_key(date_added).
common_key(date_updated).
common_key(title).
common_key(summary).
common_key(description).
common_key(status).
common_key(language).
common_key(tags).
common_key(labels).
common_key(aliases).
common_key(keywords).
common_key(identifiers).
common_key(sources).
common_key(evidence).
common_key(temporal).
common_key(provenance).
common_key(assessment).
common_key(verification).
common_key(handling).
common_key(lineage).
common_key(quality).
common_key(workflow).
common_key(geospatial).
common_key(attachments).
common_key(related_ids).
common_key(notes).
common_key(schema_org).
common_key(data).
common_key(extensions).
% tenant_id is server-private and survives internally; HTTP strips it.
common_key(tenant_id).

legacy_alias_key(type).
legacy_alias_key(tenant).
legacy_alias_key(source_dataset).
legacy_alias_key(sourceDataset).
legacy_alias_key(dateAdded).
legacy_alias_key(dateUpdated).

split_top_level([], [], []).
split_top_level([Key-Value|Rest], Known, Extra) :-
    ( common_key(Key) ->
        Known = [Key-Value|KnownRest],
        Extra = ExtraRest
    ; legacy_alias_key(Key) ->
        Known = KnownRest,
        Extra = ExtraRest
    ;
        Known = KnownRest,
        Extra = [Key-Value|ExtraRest]
    ),
    split_top_level(Rest, KnownRest, ExtraRest).

existing_data(Doc, Data) :-
    get_dict(data, Doc, Candidate),
    is_dict(Candidate),
    !,
    Data = Candidate.
existing_data(_, _{}).

existing_lineage(Doc, Lineage) :-
    get_dict(lineage, Doc, Candidate),
    is_dict(Candidate),
    !,
    Lineage = Candidate.
existing_lineage(_, _{}).

base_v09(Id, Dataset, Dtype, Added, Updated, Version, Base) :-
    schema_org_metadata(Dtype, Id, SchemaOrg),
    Base = _{
        '_id':Id,
        dataset:Dataset,
        dtype:Dtype,
        schema_version:"0.9.0",
        version:Version,
        date_added:Added,
        date_updated:Updated,
        title:"",
        summary:"",
        description:"",
        status:"recorded",
        language:"en",
        tags:[],
        labels:[],
        aliases:[],
        keywords:[],
        identifiers:[],
        sources:[],
        evidence:[],
        temporal:_{},
        provenance:_{},
        assessment:_{},
        verification:_{status:"unverified", verified:false},
        handling:_{visibility:"public", sensitive:false, pii:false},
        lineage:_{},
        quality:_{},
        workflow:_{},
        geospatial:_{},
        attachments:[],
        related_ids:[],
        notes:[],
        schema_org:SchemaOrg,
        data:_{},
        extensions:_{}
    }.

canonicalize_v09(Doc, From, Migrated) :-
    document_id(Doc, Id),
    document_dataset(Doc, Dataset),
    document_tenant(Doc, Tenant),
    document_dtype(Doc, Dtype),
    document_dates(Doc, Added, Updated),
    canonical_version(Doc, Version),
    dict_pairs(Doc, _, Pairs),
    split_top_level(Pairs, KnownPairs, ExtraPairs),
    dict_create(Known, json, KnownPairs),
    dict_create(Extra, json, ExtraPairs),
    base_v09(Id, Dataset, Dtype, Added, Updated, Version, Base),
    put_dict(Known, Base, Stage0),
    existing_data(Doc, ExistingData),
    put_dict(Extra, ExistingData, Data),
    existing_lineage(Doc, ExistingLineage),
    put_dict(migration_from, ExistingLineage, From, Lineage),
    put_dict(_{
        '_id':Id,
        dataset:Dataset,
        dtype:Dtype,
        schema_version:"0.9.0",
        version:Version,
        date_added:Added,
        date_updated:Updated,
        data:Data,
        lineage:Lineage
    }, Stage0, Stage1),
    ( Tenant = "default" ->
        ( del_dict(tenant_id, Stage1, _, Migrated) -> true ; Migrated = Stage1 )
    ; put_dict(tenant_id, Stage1, Tenant, Migrated)
    ).

migrate_to_current(Doc, From, Migrated) :-
    current_schema_version(Current),
    detected_schema(Doc, From),
    From \= Current,
    From \= "unknown",
    migration_path(From, Current, _),
    canonicalize_v09(Doc, From, Migrated).

view_scope(Doc, Tenant, Dataset) :-
    document_tenant(Doc, Tenant),
    document_dataset(Doc, Dataset).

map_view(version_distribution, Doc, Rows) :-
    !,
    ( is_dict(Doc),
      \+ design_document(Doc),
      view_scope(Doc, Tenant, Dataset),
      detected_schema(Doc, Version),
      document_dtype(Doc, Dtype)
    -> Rows = [[[Tenant, Dataset, Version, Dtype], 1]]
    ;  Rows = []
    ).
map_view(outdated_by_version, Doc, Rows) :-
    !,
    ( is_dict(Doc),
      \+ design_document(Doc),
      view_scope(Doc, Tenant, Dataset),
      document_id(Doc, Id),
      document_dtype(Doc, Dtype),
      detected_schema(Doc, From),
      current_schema_version(To),
      From \= To,
      From \= "unknown",
      migration_path(From, To, _)
    -> Rows = [[[Tenant, Dataset],
                _{id:Id, dtype:Dtype, from_schema:From, to_schema:To}]]
    ;  Rows = []
    ).
map_view(promote_current, Doc, Rows) :-
    !,
    ( is_dict(Doc),
      \+ design_document(Doc),
      view_scope(Doc, Tenant, Dataset),
      migrate_to_current(Doc, _, Migrated)
    -> Rows = [[[Tenant, Dataset], Migrated]]
    ;  Rows = []
    ).
map_view(_, _, []).

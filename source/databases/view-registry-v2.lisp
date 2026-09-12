(in-package :star.databases.couchdb)

(defparameter +v2-view-categories+
  '(("analytics" . "analytics-")
    ("research" . "research-")
    ("targets" . "targets-v2-")
    ("graph" . "graph-")
    ("geo" . "geo-")
    ("operations" . "operations-")
    ("migrations" . "migrations-"))
  "Stable HTTP category to registry-name prefixes for the v2 view API.")

(defun register-v2-view-specs ()
  "Register the second-generation StarIntel analytics and graph views."
  ;; analytics
  (register-view-spec 'analytics-count-by-dataset-dtype "analytics_v2" "count_by_dataset_dtype"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-docs-by-day-dtype "analytics_v2" "docs_by_day_dtype"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-confidence-by-dtype "analytics_v2" "confidence_by_dtype"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-quality-by-dtype "analytics_v2" "quality_by_dtype"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-sources-by-dtype "analytics_v2" "sources_by_dtype"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-evidence-by-dtype "analytics_v2" "evidence_by_dtype"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-verification-status-by-dtype "analytics_v2" "verification_status_by_dtype"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-workflow-status-by-dtype "analytics_v2" "workflow_status_by_dtype"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-events-by-kind "analytics_v2" "events_by_kind"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-events-by-status "analytics_v2" "events_by_status"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-claims-by-status "analytics_v2" "claims_by_status"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-claims-by-polarity "analytics_v2" "claims_by_polarity"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-financial-amount-by-type-currency "analytics_v2" "financial_amount_by_type_currency"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-contracts-obligated-by-currency "analytics_v2" "contracts_obligated_by_currency"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-lobbying-income-by-currency "analytics_v2" "lobbying_income_by_currency"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-lobbying-expenses-by-currency "analytics_v2" "lobbying_expenses_by_currency"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-hosts-by-asn "analytics_v2" "hosts_by_asn"
                      :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'analytics-domains-by-registrar "analytics_v2" "domains_by_registrar"
                      :reducer-p t :default-reduce t :default-include-docs nil)

  ;; research / Auto-Dig
  (register-view-spec 'research-pass-by-agent "research_v2" "pass_by_agent" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-pass-findings "research_v2" "pass_findings" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-pass-unresolved "research_v2" "pass_unresolved" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-pass-sources "research_v2" "pass_sources" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-pass-iterations "research_v2" "pass_iterations" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-node-status "research_v2" "node_status" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-node-actor-status "research_v2" "node_actor_status" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-node-requests "research_v2" "node_requests" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-node-actor-runs "research_v2" "node_actor_runs" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-node-cost "research_v2" "node_cost" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-node-elapsed "research_v2" "node_elapsed" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-node-depth "research_v2" "node_depth" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-node-failures "research_v2" "node_failures" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-node-dependency-fanout "research_v2" "node_dependency_fanout" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-node-output-count "research_v2" "node_output_count" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'research-node-children "research_v2" "node_children" :default-include-docs nil)
  (register-view-spec 'research-node-dependencies "research_v2" "node_dependencies" :default-include-docs nil)
  (register-view-spec 'research-node-targets "research_v2" "node_targets" :default-include-docs nil)
  (register-view-spec 'research-node-outputs "research_v2" "node_outputs" :default-include-docs nil)

  ;; targets
  (register-view-spec 'targets-v2-by-root "targets_v2" "by_root" :default-include-docs nil)
  (register-view-spec 'targets-v2-status-by-root "targets_v2" "status_by_root" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'targets-v2-by-actor-status "targets_v2" "by_actor_status" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'targets-v2-by-target-type "targets_v2" "by_target_type" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'targets-v2-depth "targets_v2" "depth" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'targets-v2-required-dtype "targets_v2" "required_dtype" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'targets-v2-source-usage "targets_v2" "source_usage" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'targets-v2-next-run "targets_v2" "next_run" :default-include-docs nil)
  (register-view-spec 'targets-v2-parent-edges "targets_v2" "parent_edges" :default-include-docs nil)
  (register-view-spec 'targets-v2-dependency-edges "targets_v2" "dependency_edges" :default-include-docs nil)

  ;; graph
  (register-view-spec 'graph-edges-by-subject "graph_v2" "edges_by_subject" :default-include-docs nil)
  (register-view-spec 'graph-edges-by-object "graph_v2" "edges_by_object" :default-include-docs nil)
  (register-view-spec 'graph-edges-by-predicate "graph_v2" "edges_by_predicate" :default-include-docs nil)
  (register-view-spec 'graph-out-degree "graph_v2" "out_degree" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'graph-in-degree "graph_v2" "in_degree" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'graph-predicate-counts "graph_v2" "predicate_counts" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'graph-predicate-degree "graph_v2" "predicate_degree" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'graph-dtype-edges "graph_v2" "dtype_edges" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'graph-self-loops "graph_v2" "self_loops" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'graph-lineage-edges "graph_v2" "lineage_edges" :default-include-docs nil)
  (register-view-spec 'graph-provenance-edges "graph_v2" "provenance_edges" :default-include-docs nil)
  (register-view-spec 'graph-evidence-edges "graph_v2" "evidence_edges" :default-include-docs nil)
  (register-view-spec 'graph-claim-evidence-edges "graph_v2" "claim_evidence_edges" :default-include-docs nil)

  ;; geo
  (register-view-spec 'geo-by-geohash-prefix "geo_v2" "by_geohash_prefix" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'geo-by-jurisdiction "geo_v2" "by_jurisdiction" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'geo-by-country "geo_v2" "by_country" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'geo-by-region "geo_v2" "by_region" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'geo-by-city "geo_v2" "by_city" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'geo-accuracy-stats "geo_v2" "accuracy_stats" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'geo-contains "geo_v2" "contains" :default-include-docs nil)
  (register-view-spec 'geo-contained-by "geo_v2" "contained_by" :default-include-docs nil)
  (register-view-spec 'geo-coordinates "geo_v2" "coordinates" :default-include-docs nil)

  ;; operations
  (register-view-spec 'operations-status "operations_v2" "status" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'operations-phase-state "operations_v2" "phase_state" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'operations-phase-by-operation "operations_v2" "phase_by_operation" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'operations-phase-dependencies "operations_v2" "phase_dependencies" :default-include-docs nil)
  (register-view-spec 'operations-capability-gaps "operations_v2" "capability_gaps" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'operations-assignments-agent "operations_v2" "assignments_agent" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'operations-assignments-actor "operations_v2" "assignments_actor" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'operations-dataset-roles "operations_v2" "dataset_roles" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'operations-post-actions "operations_v2" "post_actions" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'operations-target-roles "operations_v2" "target_roles" :reducer-p t :default-reduce t :default-include-docs nil)

  ;; migrations
  (register-view-spec 'migrations-version-distribution "migrations_v2" "version_distribution" :reducer-p t :default-reduce t :default-include-docs nil)
  (register-view-spec 'migrations-outdated-by-version "migrations_v2" "outdated_by_version" :default-include-docs nil)
  (register-view-spec 'migrations-promote-next "migrations_v2" "promote_next" :default-include-docs nil)
  t)

(register-v2-view-specs)

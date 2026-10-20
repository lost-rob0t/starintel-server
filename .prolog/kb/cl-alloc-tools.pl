% Durable knowledge for tools/cl-alloc-tools.lisp: SBCL allocation
% accounting, weak-registry leak hunting, bounded streaming.
% All facts below verified by machine evidence on SBCL 2.6.0 (nix build,
% 2026-10-19) unless a commit is cited.

% --- weak registry semantics ---

root_cause(weak_census_undercounts, wrong_hash_weakness,
    "An eq hash table for object->metadata leak tracking must use
     :weakness :key. With :weakness :value the ENTRY dies when the VALUE
     dies: freshly consed metadata is referenced by nothing but the
     table itself, so entries (even for live objects) vanish after any
     GC and the census undercounts. :weakness :key drops entries when
     the tracked OBJECT dies, which holds metadata strongly - exactly
     the leak-hunting contract. Symptom seen: live tracked string
     disappeared from census while a dead buffer survived one GC later.").

invariant(registry_never_leaks_what_it_tracks,
    "The :weakness :key table keeps only metadata alive; re-tracking
     replaces metadata (one row per object, not a multiset). Dead
     tracked objects disappear from census after a full GC - the
     registry cannot cause the leak it hunts.").

method(deterministic_dead_object_tests,
    "SBCL scans registers and stack conservatively, so objects made
     dead in the CENSUSING frame (setf nil) or in returned-to caller
     frames often SURVIVE a full GC. Deterministic 'dead object'
     evidence requires spawning them in a JOINED thread: registers and
     stack of a terminated thread are gone. New threads start from the
     ROOT binding of specials, so forward dynamically-bound state
     (e.g. *registry*) into the thread body with an explicit LET.").

% --- net-retained accounting semantics ---

invariant(retained_delta_semantics,
    "sb-kernel:dynamic-usage delta around two full GCs measures NET
     retained bytes, not gross allocation: collecting bodies report ~0,
     releasing bodies report NEGATIVE deltas. Assertions must accept
     minusp for release scenarios. Objects optimized dead by the
     compiler (last-reference LET in safe code) retain ~nothing.").

% --- profiling API availability on this host ---

sbcl_contrib(sb_analyzer, unavailable,
    "(require :sb-analyzer) fails: 'Don't know how to REQUIRE
     SB-ANALYZER' on the nix sbcl-with-packages build (SBCL 2.6.0).
     Do not build allocation profiling against sb-analyzer here;
     sb-sprof and sb-vm:map-allocated-objects are the loadable paths.").
sbcl_contrib(sb_sprof, available, "CPU-mode sampling profiler, loadable").
sbcl_api(map_allocated_objects,
    "(sb-vm:map-allocated-objects (lambda (obj widetag size) ...) :all)
     works with spaces :dynamic and :all (:dynamic counted 1,789,968
     objects / 121,558,432 bytes on an idle image). widetag-name is NOT
     interned in SB-VM or SB-KERNEL on this build; derive type labels
     via type-of or an explicit widetag table.").

root_cause(class_of_reader_error, sb_kernel_class_of_inherited,
    "sb-kernel:class-of does not READ: CLASS-OF is INHERITED into
     SB-KERNEL from the CL package, not external, so a double-colon-free
     reference signals 'not external in the SB-KERNEL package'. Use
     cl:class-of (same function). Verified SBCL 2.6.0.").

invariant(array_class_names_are_sb_kernel_internals,
    "(class-name (class-of v)) for specialized arrays returns
     SB-KERNEL-internal symbols, e.g. SB-KERNEL::SIMPLE-ARRAY-
     UNSIGNED-BYTE-8, not CL names. Tests and heap-diff consumers must
     match by SYMBOL-NAME, never by CL package symbols.").

method(sprof_contrib_load_order,
    "(require :sb-sprof) must complete before the reader sees
     sb-sprof: symbols: a single --eval string is READ as a whole before
     the require executes, so package references fail with 'Package
     SB-SPROF does not exist'. Put the require in an earlier --eval (or
     file prologue) than any sb-sprof: form. with-profiling (:reset t
     :mode :cpu :report :flat :loop nil) verified working; report goes
     to *standard-output* - bind it to redirect.").

method(heap_snapshot_minimal_allocation,
    "Under map-allocated-objects (GC inhibited) the census callback must
     barely allocate: an eq hash keyed by CLASS objects with pre-sized
     table and fixnum-only (count . bytes) cells walks 1.8M objects
     safely. Attribution verified exact: a 50,000-element octet vector
     was reported as 50,016 grown bytes via with-heap-growth.").

method(prebuilt_closure_probe,
    "To run ad-hoc Lisp against the full loaded server: take the wrapped
     sbcl path out of the built runner (grep -oE '/nix/store/...-sbcl-
     with-packages/bin/sbcl' in $(nix build --no-link --print-out-paths
     .#star-unit-tests)/bin/star-unit-tests), replicate its env (fresh
     HOME, XDG_CACHE_HOME under it, TMPDIR=/tmp, the script's
     LD_LIBRARY_PATH, STARINTEL_SOURCE_ROOT) and invoke with
     --no-userinit --no-sysinit --eval '(require :asdf)' then --load
     probes. nix develop does NOT work for this: it lacks ASDF output
     translations, and setting ASDF_OUTPUT_TRANSLATIONS does not save it
     because cffi-grovel writes temp files NEXT TO SOURCE in the
     read-only store (permission denied). Verified 2026-10-20 running
     tools/probe-findings.lisp against the prebuilt starintel-gserver
     closure.").

% --- bounded streaming ---

invariant(bounded_queue_backpressure,
    "The bounded queue (mutex + not-full/not-empty waitqueues) parks the
     producer when full, so run-pipeline memory is O(capacity) however
     fast the producer runs; popped slots are nil-cleared so the queue
     never retains consumed items. Producer errors are re-signaled in
     the consuming thread after close-and-drain (+queue-eof+ sentinel).
     The sentinel symbol must be compared EQ against the package-owned
     symbol: reading +queue-eof+ in a different package creates a
     distinct symbol and EQ fails (it is exported for this reason).").

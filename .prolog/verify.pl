% Task-specific verification for the current worktree.
:- set_prolog_flag(unknown, error).
:- use_module(library(plunit)).
:- use_module(library(time)).
:- ensure_loaded('facts.kb').

load_current_run :-
    repo_state(Head, _),
    atomic_list_concat(['runs/run-', Head, '.pl'], RunFile),
    ( exists_file(RunFile) -> ensure_loaded(RunFile) ; true ).

:- load_current_run.

current_successful_observation :-
    repo_state(Head, Digest),
    observation(_, _, exit(0), _, Head, Digest).

current_research_evidence :-
    research_required(false).
current_research_evidence :-
    research_required(true),
    repo_state(Head, Digest),
    brave_search(_, _, _, Head, Digest).

base_complete :-
    task(_),
    current_successful_observation,
    current_research_evidence.

% Extend this predicate with task-specific requirements and invariants.

% RED evidence lives in the merge-head run file (suite exit 1 before the
% fix, twice) plus the attribution run at parent 8533dc4 narrated there.
red_recorded_at_merge_head :-
    ( exists_file('runs/run-1599100ec1c9267d773573fdd9d679d00791106c.pl')
    -> ensure_loaded('runs/run-1599100ec1c9267d773573fdd9d679d00791106c.pl')
    ; true ),
    observation(_, command(['env',_,_,'nix','run','.#star-integration-tests']),
                exit(1), _, '1599100ec1c9267d773573fdd9d679d00791106c', _).

% GREEN: full integration suite passes at the fixed head.
green_suite_at_fixed_head :-
    repo_state(Head, Digest),
    observation(_, command(['env',_,_,'nix','run','.#star-integration-tests']),
                exit(0), _, Head, Digest).

% Mirror master fast-forwarded to canonical (no force anywhere).
mirror_master_fast_forwarded :-
    observation(_, command(['git','push','github',
                            'refs/remotes/origin/master:refs/heads/master']),
                exit(0), _, _, _).

% No recorded command performed any force push.
no_forced_pushes :-
    \+ ( observation(_, command(Args), _, _, _, _),
         member(A, Args),
         ( sub_atom(A, 0, 1, _, '+')
         ; sub_atom(A, _, _, _, '--force')
         ; sub_atom(A, _, _, _, 'force-with-lease') ) ).

complete :-
    base_complete,
    red_recorded_at_merge_head,
    green_suite_at_fixed_head,
    mirror_master_fast_forwarded,
    no_forced_pushes.

:- begin_tests(workspace_verification).

test(complete) :-
    complete.

:- end_tests(workspace_verification).

main :-
    catch(call_with_time_limit(30, (run_tests, once(complete))),
          Error,
          (print_message(error, Error), fail)),
    !,
    halt(0).
main :-
    halt(1).

:- initialization(main, main).

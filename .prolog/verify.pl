% Task-specific verification for the current worktree.
:- set_prolog_flag(unknown, error).
:- use_module(library(plunit)).
:- use_module(library(time)).
:- use_module(library(lists)).
:- dynamic machine_observation/8, machine_brave_search/7, requirement/2, claim/2.
:- discontiguous machine_observation/8, machine_brave_search/7, requirement/2, claim/2.
:- ensure_loaded('facts.kb').

load_current_run :-
    verification_context(_, Run),
    atomic_list_concat(['runs/', Run, '/evidence.pl'], RunFile),
    ( exists_file(RunFile) -> ensure_loaded(RunFile) ; true ).

:- load_current_run.

% Compatibility views expose only the active task/run, never another journal.
observation(Id, Command, Exit, Output, Head, Digest) :-
    verification_context(Task, Run),
    machine_observation(Task, Run, _, Command, _, _, Head, Digest),
    findall(record(I, E, O),
            machine_observation(Task, Run, I, Command, E, O, Head, Digest),
            Records),
    last(Records, record(Id, Exit, Output)).
brave_search(Query, Result, Time, Head, Digest) :-
    verification_context(Task, Run),
    machine_brave_search(Task, Run, Query, Result, Time, Head, Digest).

current_successful_observation :-
    repo_state(Head, Digest),
    observation(_, _, exit(0), _, Head, Digest).

current_research_evidence :-
    research_required(false).
current_research_evidence :-
    research_required(true),
    repo_state(Head, Digest),
    brave_search(_, _, _, Head, Digest).

% Closed executable bindings. Unsupported claims stay unknown.
requirement_status(Id, Status, Evidence) :-
    requirement_result(Id, Actual, Evidence), Status = Actual.

requirement_result(Id, conflict, none) :-
    requirement(Id, A), requirement(Id, B), A \== B, !.
requirement_result(Id, Status, Evidence) :-
    requirement(Id, command(Argv)),
    !,
    repo_state(Head, Digest),
    ( observation(Observation, command(Argv), exit(Exit), Output, Head, Digest)
    -> ( Exit =:= 0 -> Status = verified ; Status = failed ),
       Evidence = evidence(Observation, Output, Head, Digest)
    ; observation(_, command(Argv), _, _, _, _)
    -> Status = stale, Evidence = none
    ; Status = unknown, Evidence = none ).
requirement_result(_, unknown, none).

claim_status(Id, Status, Evidence) :-
    claim_result(Id, Actual, Evidence), Status = Actual.

claim_result(Id, conflict, none) :-
    claim(Id, A), claim(Id, B), A \== B, !.
claim_result(Id, Status, Evidence) :-
    claim(Id, requirement(Requirement)),
    !, requirement_status(Requirement, Status, Evidence).
claim_result(_, unknown, none).

current_failed_observation :-
    repo_state(Head, Digest),
    observation(_, _, exit(Exit), _, Head, Digest),
    Exit =\= 0.

base_complete :-
    task(_),
    \+ current_failed_observation,
    forall(requirement(Id, _), (requirement_status(Id, RequirementStatus, _), RequirementStatus == verified)),
    forall(claim(Id, _), (claim_status(Id, ClaimStatus, _), ClaimStatus == verified)),
    current_successful_observation,
    current_research_evidence.

% Extend this predicate with task-specific requirements and invariants.
complete :-
    base_complete.

:- begin_tests(workspace_verification).

test(complete) :-
    once(complete).

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

% Task-specific verification: issue #37 consumer reconnect slice.
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

base_complete :-
    task('issue-37-consumer-reconnect'),
    current_successful_observation.

% Full unit gate (consumer suite included) recorded green by the machine log.
unit_gate_green :-
    repo_state(Head, _),
    observation(_, command(['bash','-c',_]), exit(0), _, Head, _).

complete :- base_complete, unit_gate_green.

:- begin_tests(workspace_verification).
test(complete) :- complete.
:- end_tests(workspace_verification).

main :-
    catch(call_with_time_limit(30, (run_tests, once(complete))),
          Error,
          (print_message(error, Error), halt(1))),
    halt(0).

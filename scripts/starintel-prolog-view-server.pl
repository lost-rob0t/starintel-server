#!/usr/bin/env swipl
% CouchDB external query server for StarIntel migration views.
% One JSON request is read from stdin and one JSON response is written to stdout.
%
% Security boundary: add_fun accepts only exact symbolic migration_view(...)
% selectors. This process never consults or calls arbitrary source received
% from CouchDB, documents, tenants, or API clients.

:- use_module(library(http/json)).
:- use_module('../.prolog/kb/migrations.pl').

:- dynamic registered_map/1.

:- initialization(main, main).

allowed_source("migration_view(version_distribution)", version_distribution).
allowed_source("migration_view(outdated_by_version)", outdated_by_version).
allowed_source("migration_view(promote_current)", promote_current).

protocol_error(Name, Reason, ["error", Name, Reason]).

reset_query_server :-
    retractall(registered_map(_)).

handle_request(["reset"|_], true) :-
    !,
    reset_query_server.
handle_request(["add_fun", Source], Reply) :-
    !,
    handle_add_fun(Source, Reply).
handle_request(["map_doc", Doc], Reply) :-
    !,
    findall(Rows,
            ( registered_map(View),
              starintel_migrations:map_view(View, Doc, Rows)
            ),
            Reply).
handle_request(["reduce"|_], Reply) :-
    !,
    protocol_error("unsupported_command",
                   "Custom Prolog reducers are disabled; use CouchDB built-ins such as _sum.",
                   Reply).
handle_request(["rereduce"|_], Reply) :-
    !,
    protocol_error("unsupported_command",
                   "Custom Prolog rereduce is disabled; use CouchDB built-ins.",
                   Reply).
handle_request(["ddoc"|_], Reply) :-
    !,
    protocol_error("unsupported_command",
                   "Design-document function execution is disabled in the migration query server.",
                   Reply).
handle_request(Request, Reply) :-
    format(string(Reason),
           "Unsupported CouchDB query-server request: ~w",
           [Request]),
    protocol_error("unsupported_command", Reason, Reply).

handle_add_fun(Source, true) :-
    string(Source),
    allowed_source(Source, View),
    !,
    assertz(registered_map(View)).
handle_add_fun(Source, Reply) :-
    string(Source),
    !,
    format(string(Reason),
           "Unsupported StarIntel migration map function: ~s",
           [Source]),
    protocol_error("unsupported_function", Reason, Reply).
handle_add_fun(_, Reply) :-
    protocol_error("unsupported_function",
                   "Unsupported StarIntel migration map function: selector must be a string.",
                   Reply).

write_reply(Reply) :-
    json_write_dict(current_output, Reply, [width(0)]),
    nl,
    flush_output(current_output).

process_request(Request) :-
    catch(handle_request(Request, Reply),
          Error,
          ( message_to_string(Error, Reason),
            protocol_error("query_server_error", Reason, Reply)
          )),
    write_reply(Reply).

read_request(Result) :-
    catch(json_read_dict(current_input,
                         Request,
                         [ value_string_as(string),
                           end_of_file(end_of_file)
                         ]),
          Error,
          Result = read_error(Error)),
    ( var(Result) -> Result = request(Request) ; true ).

query_loop :-
    read_request(Result),
    ( Result = request(end_of_file)
    -> true
    ; Result = read_error(Error)
    -> message_to_string(Error, Reason),
       protocol_error("invalid_json", Reason, Reply),
       write_reply(Reply),
       query_loop
    ; Result = request(Request),
      process_request(Request),
      query_loop
    ).

main(_) :-
    set_prolog_flag(verbose, silent),
    reset_query_server,
    query_loop.

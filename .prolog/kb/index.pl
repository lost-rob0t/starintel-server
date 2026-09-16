% Durable project knowledge for starintel-server. Load with:
%   swipl -g "use_module('.prolog/kb/index.pl')"
:- multifile root_cause/3, invariant/2, method/2.
:- ensure_loaded('root-causes.pl').
:- ensure_loaded('tooling.pl').
:- ensure_loaded('git-remotes.pl').
:- ensure_loaded('ulid-quarantine.pl').

%%% File    : user_default.erl

-module(user_default).

-export([help/0]).
-export([q/0, halt/0]).
-export([p/1]).
-export([pid/1,pid/2]).

%% info
-export([i/0, i/1, ni/0, bt/1]).
-export([pi/0, pi/1, pi/2, pi2/0]).

%% compile
-export([c/1, c/2]).
-export([sm/0, smc/0]).

%% load
-export([lm/0, mm/0]).

%% call
-export([cl/3]).

%% doc
-export([src/3]).


%%%
%%% See:
%%% http://www.erlang.org/pipermail/erlang-questions/2006-September/022750.html
%%%
help() ->
    shell_default:help(),
    io:format(help_text()),
    true.

help_text() ->
    "** user extended commands **\n"
        "i()           -- short version of c:i()\n"
        "ni()          -- i/0 on all connected nodes\n"
        "lm()          -- load all changed modules\n"
        "cl(M,F,A)     -- call unexported function\n"
        "nl()          -- load all changed modules on all known nodes\n"
        "mm()          -- list modified modules\n"
    .

q()    -> warn_quit_msg().
halt() -> warn_quit_msg().

warn_quit_msg() ->
    "Kill the system? Use init:stop() (or C-c C-c for remote node).".


p(Term) ->
    io:format("~p\n", [Term]).


%% ---------------------------------------------------------------------------
%% Info

i() -> user_info:i().
i(Pid) -> user_info:i(Pid).

ni() -> user_info:ni().

bt(Pid)  -> user_info:bt(Pid).

pi() -> user_info:pi().
pi(Id) -> user_info:pi(Id).
pi(X,Y) -> user_info:pi(X, Y).
pi2() -> user_info:pi2().


pid(I2,I3)                    -> pid({I2,I3}).
pid({I1,I2,I3})               -> c:pid(I1,I2,I3);
pid({I2,I3})                  -> pid({0,I2,I3});
pid(Pid)  when is_pid(Pid)    -> Pid;
pid(Atom) when is_atom(Atom)  -> whereis(Atom);
pid(I2)   when is_integer(I2) -> pid({0,I2,0});
pid(Str)  when hd(Str)==$<    -> list_to_pid(Str);
pid(Str)  when is_list(Str)   -> pid("<"++Str++">").

%% ---------------------------------------------------------------------------
%% Compile

c(M) -> user_compile:c(M).

c(M, Opts) -> user_compile:c(M, Opts).

smc() -> user_compile:smc().

sm() -> user_compile:sm().

%% ---------------------------------------------------------------------------
%% Load

mm() -> user_load:mm().

lm() -> user_load:lm().

%% ---------------------------------------------------------------------------
%% Call

cl(M,F,A) ->
    user_call:cl(M,F,A).


%% ---------------------------------------------------------------------------
%% Doc
src(M,F,A) ->
    shell_doc:print_mfa(M,F,A).

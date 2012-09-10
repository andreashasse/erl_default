%%% @author Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created :  6 Sep 2012 by Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% with help from uwiger:
%%% http://erlang.org/pipermail/erlang-questions/2012-March/064801.html

-module(shell_doc).

-export([print_mfa/3, print_mfa/4, api/1, api/2]).

api(M) -> api(M, head).

api(M, Format) ->
    Exports = [D || {Func, _} = D <-
                        proplists:get_value(exports, M:module_info(), []),
                    Func =/= module_info],
    lists:foreach(fun({F, A}) -> print_mfa(M, F, A, Format) end, Exports).

-spec print_mfa(M::module(), F::atom(), A::integer()) -> ok.
print_mfa(M, F, A) ->
    print_mfa(M, F, A, all).

print_mfa(M, F, A, Format) ->
    AbsCode = abs_code(M),
    case find_function(AbsCode, F, A) of
        {_, []} -> io:fwrite("not found~n");
        {Attr, [Function]} ->
            case Format of
                all  -> print_code(Attr ++ [Function]);
                head -> print_one_line_fun(Function);
                src  -> print_code([Function]);
                spec -> print_code(Attr)
            end
    end.

find_function(AbsCode, F, A) ->
    {lists:filter(fun(Code) -> do_find_attr(Code, F, A) end, AbsCode),
     lists:filter(fun(Code) -> do_find_function(Code, F, A) end, AbsCode)}.

do_find_attr({attribute, _Line, spec, {{F, A}, _}}, F, A) -> true;
do_find_attr(_, _, _) -> false.

do_find_function({function, _Line, F, A, _}, F, A) -> true;
do_find_function(_, _, _) -> false.

abs_code(M) ->
    {ok, {M, [{abstract_code, {_, AbsCode}}]}} =
        beam_lib:chunks(code:which(M), [abstract_code]),
    epp:restore_typed_record_fields(AbsCode).


%% ---------------------------------------------------------------------------
%% Printing
print_code(Code) ->
    io:fwrite("~s~n", [lists:flatten(
                         [erl_pp:form(Form) ||
                             Form <- Code])]).

print_one_line_fun({function, _Line, F, _Arity, Clauses}) ->
    Lines = lists:map(fun do_print_one_line_fun/1, Clauses),
    [Line|_] = lists:sort(fun(L1, L2) -> length(L1) > length(L2) end, Lines),
    io:fwrite("~p(~s)~n", [F, Line]).

do_print_one_line_fun({clause, _CLine, Args, _Guards,  _Body}) ->
    string:join(lists:map(fun erl_prettypr:format/1, Args), ", ").

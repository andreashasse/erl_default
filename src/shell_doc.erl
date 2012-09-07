%%% @author Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created :  6 Sep 2012 by Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% with help from uwiger:
%%% http://erlang.org/pipermail/erlang-questions/2012-March/064801.html

-module(shell_doc).

-export([print_mfa/3]).

-spec print_mfa(M::module(), F::atom(), A::integer()) -> ok.
print_mfa(M, F, A) ->
    AbsCode = abs_code(M),
    Function = find_function(AbsCode, F, A),
    print_code(Function).

print_code(Code) ->
    io:fwrite("~s~n", [lists:flatten(
                         [erl_pp:form(Form) ||
                             Form <- Code])]).

find_function(AbsCode, F, A) ->
    lists:filter(fun(Code) -> do_find_function(Code, F, A) end, AbsCode).

do_find_function({function, _Line, F, A, _}, F, A) -> true;
do_find_function({attribute, _Line, spec, {{F, A}, _}}, F, A) -> true;
do_find_function(_, _, _) -> false.

abs_code(M) ->
    {ok, {M, [{abstract_code, {_, AbsCode}}]}} =
        beam_lib:chunks(code:which(M), [abstract_code]),
    epp:restore_typed_record_fields(AbsCode).

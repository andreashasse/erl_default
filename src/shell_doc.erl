%%% @author Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created :  6 Sep 2012 by Andreas Hasselberg <andreas.hasselberg@gmail.com>

-module(shell_doc).

-export([print_fun/3]).

-spec print_fun(M::module(), F::atom(), A::integer()) -> ok.
print_fun(M, F, A) ->
    AbsCode = abs_code(M),
    Function = find_function(AbsCode, F, A),
    print_code(Function).

print_code(Code) ->
    io:fwrite("~s~n", [erl_prettypr:format(erl_syntax:form_list(Code))]).

find_function(AbsCode, F, A) ->
    lists:filter(fun(Code) -> do_find_function(Code, F, A) end, AbsCode).

do_find_function({function, _Line, F, A, _}, F, A) -> true;
do_find_function({attribute, _Line, spec, {{F, A}, _}}, F, A) -> true;
do_find_function(_, _, _) -> false.

abs_code(M) ->
    {ok, {M, [{abstract_code, {_, AbsCode}}]}} =
        beam_lib:chunks(code:which(M), [abstract_code]),
    AbsCode.
 

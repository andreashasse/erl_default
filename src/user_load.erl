-module(user_load).

-export([lm/0, mm/0]).

%% Code provided by Vladimir Sekissov <svg@surnet.ru>

mm() ->
    modified_modules().

lm() ->
    [c:l(M) || M <- mm()].

modified_modules() ->
    [M || {M, _} <- code:all_loaded(), module_modified(M)].

module_modified(Module) ->
    case code:is_loaded(Module) of
        {file, preloaded} ->
            false;
        {file, Path} ->
            CompileOpts = proplists:get_value(compile, Module:module_info()),
            CompileTime = proplists:get_value(time, CompileOpts),
            Src = proplists:get_value(source, CompileOpts),
            module_modified(Path, CompileTime, Src);
        _ ->
            false
    end.


module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
        false ->
            false;
        ModPath ->
            case beam_lib:chunks(ModPath, ["CInf"]) of
                {ok, {_, [{_, CB}]}} ->
                    CompileOpts =  binary_to_term(CB),
                    CompileTime = proplists:get_value(time, CompileOpts),
                    Src = proplists:get_value(source, CompileOpts),
                    not (CompileTime == PrevCompileTime) and (Src == PrevSrc);
                _ ->
                    false
            end
    end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
        {ok, _} ->
            Path;
        _ ->
            %% may be the path was changed?
            case code:where_is_file(filename:basename(Path)) of
                non_existing ->
                    false;
                NewPath ->
                    NewPath
            end
    end.

%%% @doc
%%% NOTE: This script does not remove configs that you remove.
%%% @end

-module(app_conf_reloader).

-export([reload/1, find_file_updates/1]).

-ignore_xref([reload/1, find_file_updates/1]).

%% -----------------------------------------------------------------
%% API

reload(FileName) -> update(find_file_updates(FileName)).

find_file_updates(FileName) ->
    {ok, [Configs]} = file:consult(FileName),
    find_updates(Configs).

%% -----------------------------------------------------------------
%% Find update

find_updates(Configs) ->
    lists:zf(fun({Appid, AppCfg}) -> find_app_updates(Appid, AppCfg) end,
             Configs).

find_app_updates(AppId, NewCfgs) ->
    CurrCfgs = application:get_all_env(AppId),
    CurrKeys = cfg_keys(CurrCfgs),
    NewKeys = cfg_keys(NewCfgs),
    AddKeys = NewKeys -- CurrKeys,
    AddCfgs =  cfgs(AddKeys, NewCfgs),
    BothKeys = NewKeys -- AddKeys,
    {ChangedCfgs, _UnChangedCfgs} = find_changed(NewCfgs, CurrCfgs, BothKeys),
    % UnChangedKeys = cfg_keys(_UnChangedCfgs),
    %% error_logger:info_msg("UnChanged config keys ~p: ~p",
    %%                       [AppId,UnChangedKeys]),
    Res = {AddCfgs, ChangedCfgs},
    case has_change(Res) of
        true  -> {true, {AppId, Res}};
        false -> false
    end.

has_change({[], []}) -> false;
has_change({_, _}) -> true.

find_changed(NewCfgs, CurrCfgs, BothKeys) ->
    lists:partition(
      fun({NewCfg, CurrCfg}) -> NewCfg =/= CurrCfg end,
      lists:zip(cfgs(BothKeys, NewCfgs), cfgs(BothKeys, CurrCfgs))).

%% -----------------------------------------------------------------
%% Update

update(Updates) ->
    [update_app(AppId, AppUpdates) || {AppId, AppUpdates} <- Updates].

update_app(AppId, {AddCfgs, ChangedCfgs}) ->
    {NewChangedCfgs, _OldChangedCfgs} = lists:unzip(ChangedCfgs),
    set(AppId, AddCfgs),
    set(AppId, NewChangedCfgs).

set(AppId, NewCfgs) ->
    error_logger:info_msg("Updating config ~p ~p", [AppId, NewCfgs]),
    [application:set_env(AppId, Key, Value) || {Key, Value} <- NewCfgs].

%% -----------------------------------------------------------------
%% Utils

cfg_keys(NewCfgs) -> lists:map(fun({Key, _Value}) -> Key end, NewCfgs).

cfgs(Keys, Cfgs) -> [cfg(Key, Cfgs) || Key <- Keys].

cfg(Key, Cfgs) -> lists:keyfind(Key, 1, Cfgs).

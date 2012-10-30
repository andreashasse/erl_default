%%% @author Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% @copyright (C) 2012, Andreas Hasselberg
%%% @doc
%%%
%%% @end
%%% Created : 28 Sep 2012 by Andreas Hasselberg <andreas.hasselberg@gmail.com>

-module(app_util).

-export([get_env/3, dev_start/1, dev_start/2]).

get_env(App, Value, Default) ->
    case application:get_env(App, Value) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

dev_start(App) -> dev_start(App, temporary).

dev_start(App, Type) ->
    case application:start(App, Type) of
        {error, {not_started, DepApp}} ->
            dev_start(DepApp),
            dev_start(App, Type);
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.

-module(sys_top).

-record(state, {last_msg :: undefined | {any(), erlang:timestamp()}, %% FIXME: type spec
                no_msg :: integer(),
                io :: atom() | list(),
                starttime = now() :: erlang:timestamp(),
                stats = dict:new() :: dict()}).

-export([install/2, install/3]).

-ignore_xref([{install, 2}, {install, 3}]). %% debug

install(Name, NoMsgs) ->
    install(Name, NoMsgs, standard_io).

install(Name, NoMsgs, IO) ->
    State = #state{io = IO, no_msg = NoMsgs},
    sys:install(Name, {fun handle_event/3, State}).

handle_event(State, {in, {'$gen_call', _From, Event}}, _ProcState) ->
    handle_in(State, Event);
handle_event(State, {in, {'$gen_cast', Event}}, _ProcState) ->
    handle_in(State, Event);
handle_event(State,{noreply, _}, _ProcState) ->
    handle_out(State);
handle_event(State,{out, _, _, _}, _ProcState) ->
    handle_out(State);
handle_event(State, Event, _ProcState) ->
    error_logger:error_msg("Not handled message ~p ~p~n", [State, Event]),
    State.

handle_in(State, Event) ->
    EventGroup = event_group(Event),
    State#state{last_msg = {EventGroup, now()}}.

handle_out(#state{last_msg = undefined} = State) -> State;
handle_out(State) ->
    #state{last_msg = {EventGroup, InTS},
           stats = Stats} = State,
    Time = timer:now_diff(now(), InTS),
    NewState = State#state{last_msg = undefined,
                           no_msg = State#state.no_msg-1,
                           stats = stats_add(Stats, EventGroup, Time)},
    if NewState#state.no_msg > 0 -> NewState;
       true -> format_state(NewState),
               done
    end.

stats_add(Stats, EventGroup, Time) ->
    dict:update(
      EventGroup,
      fun({Time0, Count0}) -> {Time0 + Time, Count0 + 1} end,
      {Time, 1},
      Stats).

format_state(#state{io = IO} = State) ->
    Dev = maybe_open(IO),
    try
        io:format(Dev, "Clock time: \t~p\n",
                  [timer:now_diff(now(), State#state.starttime)]),
        lists:foreach(
          fun(Ev) -> print_event_stat(Dev, Ev) end,
          lists:keysort(2, dict:to_list(State#state.stats)))
    after
        maybe_close(Dev)
    end.

maybe_open(Standard) when is_atom(Standard) ->
    Standard;
maybe_open(Fname) when is_list(Fname) ->
    case file:open(Fname, [write]) of
        {ok, Dev} -> Dev;
        Error -> error_logger:error_msg("Error when opening file ~p", [Error])
    end.

maybe_close(Dev) when is_pid(Dev) ->
    file:close(Dev);
maybe_close(_Dev) -> ok.

print_event_stat(Dev, {Name, {Time, Count}}) ->
    io:format(Dev,"~p:\t ~p\t ~p~n", [Name, Time, Count]).

event_group(Event) when is_atom(Event) -> Event;
event_group(Event) when is_tuple(Event) -> element(1, Event).


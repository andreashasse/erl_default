-module(user_info).

-export([i/0, i/1, ifilter/1, ni/0, bt/1]).
-export([pi/0, pi/1, pi/2, pi2/0]).

%% Good ol' i() but includes zooombie support
i() -> i1(processes()).
ni() -> i1(all_procs()).

ifilter(Filters) ->
    FilterFuns = lists:map(fun filter_fun/1, Filters),
    F = fun(Info) -> lists:all(fun(F) -> F(Info) end, FilterFuns) end,
    i1(processes(), F).

filter_fun({linked_to, Pid0}) ->
    Pid = user_default:pid(Pid0),
    fun(Info) -> lists:member(Pid, proplists:get_value(links, Info)) end;
filter_fun(active) ->
    fun(Info) -> waiting =/= proplists:get_value(status, Info) end;
filter_fun({min_msg, No}) ->
    fun(Info) -> No =< proplists:get_value(message_queue_len, Info) end;
filter_fun(F) when is_function(F, 1) -> F.

i(X) ->
  case user_default:pid(X) of
    P when is_pid(P) -> pinfo(P);
    P -> P
  end.

%% i(Pid) when is_pid(Pid) -> pinfo(Pid);
%% i(Name) when is_atom(Name) ->
%%     case whereis(Name) of
%% 	undefined -> undefined;
%% 	Pid -> i(Pid)
%%     end.

%%i(X,Y,Z) -> pinfo(c:pid(X,Y,Z)).

%% Code moified from c.erl
i1(Ps) -> i1(Ps, fun(_) -> true end).

i1(Ps, Filter) ->
    Alive = lists:filter(fun palive/1, Ps),
    i2(Alive, Filter),
    case lists:filter(fun pzombie/1, Ps) of
        [] ->
            ok;
        Zombies ->
            %% Zombies is not the same as Ps-Alive, since the remote
            %% process that fetched Ps is included among Alive, but has
            %% exited (for ni/0).
            io:format("\nDead processes:\n"),
            i2(Zombies, Filter)
    end.

i2(Ps, Filter) ->
    iformat("Pid", "Initial Call", "Current Function", "Reds", "Msgs"),
    {{RF,MF},{R,M}} = lists:foldl(
                        fun(Pid, Acc) -> display_info(Pid, Acc, Filter) end,
                        {{0,0},{0,0}}, Ps),
    iformat("Filter", "", "", io_lib:write(RF), io_lib:write(MF)),
    iformat("Total", "", "", io_lib:write(R), io_lib:write(M)).

palive(Pid) ->
    case pinfo(Pid, status) of
        undefined         -> false;
        {status, exiting} -> false;
        _                 -> true
    end.

pzombie(Pid) ->
    case pinfo(Pid, status) of
        undefined         -> false;
        {status, exiting} -> true;
        _                 -> false
    end.

pinfo(Pid) ->
    N = node(Pid),
    case
        case N =:= node() of
            true -> [];
            false-> [{node,N}]
        end ++
        case rpc:call(N,erlang,process_info,[Pid]) of
            L when is_list(L) -> L;
            _ -> []
        end of
        [] -> [];
        I -> [{pid,Pid}|I]
    end.

pinfo(Pid, Item) ->
    case is_alive() of
        true -> rpc:call(node(Pid), erlang, process_info, [Pid, Item]);
        false -> process_info(Pid, Item)
    end.

all_procs() ->
    case is_alive() of
        true -> lists:flatmap(fun (N) -> rpc:call(N, erlang, processes, []) end,
                              [node() | nodes()]);
        false -> processes()
    end.

display_info(Pid, {{RF,MF}, {R,M}}, Filter) ->
    case pinfo(Pid) of
        [] ->
            {R, M};
        Info ->
            Reds = fetch(reductions, Info),
            LM = fetch(message_queue_len, Info),
            case Filter(Info) of
                true ->
                    Call = initial_call(Info),
                    Curr = fetch(current_function, Info),
                    iformat(io_lib:write(Pid),
                            mfa_string(Call),
                            mfa_string(Curr),
                            io_lib:write(Reds),
                            io_lib:write(LM)),
                    {{RF+Reds, MF+LM},{R+Reds, M+LM}};
                false ->
                    {{RF, MF},{R+Reds, M+LM}}
            end
    end.

%% We can do some assumptions about the initial call.
%% If the initial call is proc_lib:init_p/5 we can find more information
%% by calling the function proc_lib:translate_initial_call/1.
initial_call(Info)  ->
    case fetch(initial_call, Info) of
        {proc_lib, init_p, 5} ->
            proc_lib:translate_initial_call(Info);
        ICall ->
            ICall
    end.

mfa_string({M, F, A}) ->
    io_lib:format("~w:~w/~w", [M, F, A]);
mfa_string(X) ->
    io_lib:write(X).

fetch(Key, Info) ->
    case lists:keysearch(Key, 1, Info) of
        {value, {_, Val}} -> Val;
        false -> 0
    end.

iformat(A1, A2, A3, A4, A5) ->
    io:format("~-12s ~-23s ~-23s ~12s ~4s\n", [A1,A2,A3,A4,A5]).


%% Port info
%% I don't really know which info is most relevent, so I included
%% both pi() and pi2().
pi() ->
    piformat("Id", "Name", "Connected", "Initial Call", "Current Function"),
    do_pi(fun(Info) ->
                  Id = fetch(id, Info),
                  Name = fetch(name, Info),
                  case fetch(connected, Info) of
                      Pid when is_pid(Pid) ->
                          {ICall, Curr} =
                              case pinfo(Pid) of
                                  [] ->
                                      {[], []};
                                  ProcInfo ->
                                      {initial_call(ProcInfo),
                                       fetch(current_function, ProcInfo)}
                              end,
                          piformat(io_lib:write(Id),
                                   Name,
                                   io_lib:write(Pid),
                                   mfa_string(ICall),
                                   mfa_string(Curr));
                      Port when is_port(Port) ->
                          piformat(io_lib:write(Id),
                                   Name,
                                   io_lib:write(Port),
                                   "","")
                  end
          end).

piformat(A1, A2, A3, A4, A5) ->
    io:format("~-6s ~-10s ~-12s ~-23s ~-23s\n", [A1,A2,A3,A4,A5]).

pi2() ->
    pi2format("Id", "Name", "Connected", "Recv", "Sent"),
    do_pi(fun(Info) ->
                  Id = fetch(id, Info),
                  Name = fetch(name, Info),
                  Pid = fetch(connected, Info),
                  Recv = fetch(input, Info),
                  Sent = fetch(output, Info),
                  pi2format(io_lib:write(Id),
                            Name,
                            io_lib:write(Pid),
                            io_lib:write(Recv),
                            io_lib:write(Sent))
          end).

pi2format(A1, A2, A3, A4, A5) ->
    io:format("~-6s ~-20s ~-12s ~-10s ~-10s\n", [A1,A2,A3,A4,A5]).

do_pi(Print) ->
    lists:foreach(
      fun(P) ->
              case erlang:port_info(P) of
                  undefined ->
                      ok;
                  Info ->
                      Print(Info)
              end
      end, erlang:ports()).

pi(Id) ->
    pi_l(erlang:ports(), Id).

pi_l([P | Ps], Id) ->
    case erlang:port_info(P, id) of
        {id, Id} ->
            erlang:port_info(P);
        _ ->
            pi_l(Ps, Id)
    end;
pi_l([], _Id) ->
    undefined.


pi(X,Y) ->
    PStr = lists:flatten(io_lib:format("#Port<~w.~w>", [X,Y])),
    pi_l2(erlang:ports(), PStr).

pi_l2([P | Ps], PStr) ->
    case lists:flatten(io_lib:format("~w", [P])) of
        PStr ->
            erlang:port_info(P);
        _ ->
            pi_l2(Ps, PStr)
    end;
pi_l2([], _PStr) ->
    undefined.

%% Doesn't do process_display, which means it can be used when
%% remotely connecting to a node.
bt(Pid) when is_pid(Pid) ->
    case pinfo(Pid, backtrace) of
        {backtrace, Bin} ->
            io:format("~s\n", [binary_to_list(Bin)]);
        _ ->
    undefined
    end.

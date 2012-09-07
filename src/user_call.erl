%%% @author Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% @copyright (C) 2012, Andreas Hasselberg
%%% @doc
%%%
%%% @end
%%% Created :  7 Sep 2012 by Andreas Hasselberg <andreas.hasselberg@gmail.com>

-module(user_call).

-export([parse_transform/2, cl/3]).

cl(M,F,A) ->
  if is_list(A) ->
      try_fun(M,F,A);
     is_tuple(A) ->
      try_fun(M,F,tuple_to_list(A));
     true ->
      try_fun(M,F,[A])
  end.

try_fun(M,F,Args)->
  export(M,F,length(Args)),
  R = apply(M,'APPLY',Args),
  code:soft_purge(M),code:load_file(M),
  R.

export(M,F,A) ->
  {_,_,B}=compile:file(user_compile:src(M),
                       [{parse_transform,user_call},
                        {export_fun,{F,A}},binary]),
  code:soft_purge(M),
  code:load_binary(M,"",B).

parse_transform(A,B) ->
  case lists:keysearch(export_fun,1,B) of
    {value,{export_fun,{F,Arity}}} ->
      Last = element(2,lists:last(A)),
      add_fun('APPLY',Arity,
              mk_fun(F,Arity,Last),A,Last+4);
    _ -> A
  end.


mk_fun(F,A,N) ->
  {function,N+1,'APPLY',A,
   [{clause,N+2,vars(A,N+2),[],
     [{call,N+3,{'fun',N+3,{function,F,A}},
       vars(A,N+3)}]}]}.

vars(0,_P) -> [];
vars(N,P) -> [{var,P,list_to_atom("V"++[$A+N])}|
              vars(N-1,P)].

add_fun(Name,Arity,Body,Code,Eof) ->
  add_eof(
    add_exp(Code,Name,Arity),Body,Eof).

add_eof([{eof,_}],Body,Eof) -> [Body,{eof,Eof}];
add_eof([N|R],Body,Eof) -> [N|add_eof(R,Body,Eof)].

add_exp([{attribute,N,export,L}|R],Name,Arity) ->
  [{attribute,N,export,[{Name,Arity}|L]}|R];
add_exp([O|R],N,A) ->
 [O| add_exp(R,N,A)];
add_exp([],_,_) -> [].


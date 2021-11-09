-module(frappe).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called frappe.

% Export at least the API:
-export([fresh/1, set/4, read/2, insert/4, update/4, upsert/3, stable/3, all_items/1,
         stop/1]).

-behaviour(gen_statem).
% You may have other exports as well
-export([ init/1,server/3]).
-export([callback_mode/0]).

fresh(Capcity) ->
  gen_statem:start(?MODULE, {server,Capcity}, []).

set(FS, Key, Value, C) ->
  gen_statem:call(FS, {set, Key, Value, C}).

read(FS, Key) ->
  gen_statem:call(FS, {read, Key}).

insert(FS, Key, Value, C) ->
  gen_statem:call(FS, {insert, Key, Value, C}).

update(FS, Key, Value, C) ->
  gen_statem:call(FS, {update, Key, Value, C}).

upsert(FS, Key, Fun) ->
  gen_statem:call(FS, {upsert, Key, Fun}).

stable(FS, Key, Ref) ->
  gen_statem:call(FS, {stable, Key, Ref}).

all_items(FS) ->
  gen_statem:call(FS, all_items).

stop(FS) ->
  gen_statem:call(FS, stop).

server({call,From},{set, Key, Value, C},{OriginalCapcity, CurrentCapcity, List})->
  case schedule(Key,From,C,OriginalCapcity,CurrentCapcity,List) of
    {exceed,[]}->keep_state_and_data;
    {scheduled1,NewC,NewList}->
      {ok,Pid}=gen_statem:start(?MODULE, {cache,Key,Value,C}, []),
      gen_statem:reply(From,ok),
      {keep_state,{OriginalCapcity,NewC,[{Key,C,Pid}|NewList]}};
    {scheduled2,NewC,NewList,{_Key1,_OldC,Pid}}->
      gen_statem:call(Pid,{set,Key,Value,C}),gen_statem:reply(From,ok),
      {keep_state,{OriginalCapcity,NewC,[{Key,C,Pid}|NewList]}}
  end;
server({call,From},{read,Key},{_OriginalCapcity, _CurrentCapcity, List})->
  case findPid(Key, List) of
    nothing->gen_statem:reply(From, nothing),keep_state_and_data;
    {ok,Pid}->Result=gen_statem:call(Pid, read),gen_statem:reply(From, {ok,Result}),keep_state_and_data
  end;
server({call,From},{insert,Key,Value,C},{OriginalCapcity, CurrentCapcity, List})->
  case schedule(Key,From,C,OriginalCapcity,CurrentCapcity,List) of
    {exceed,[]}->keep_state_and_data;
    {scheduled1,NewC,NewList}-> 
      {ok,Pid}=gen_statem:start(?MODULE, {cache,Key,Value,C}, []),
      gen_statem:reply(From,ok),
      {keep_state,{OriginalCapcity,NewC,[{Key,C,Pid}|NewList]}};
    {scheduled2,NewC,NewList,{_Key1,_OldC,Pid}}->
      % Here is to Judge whether can insert immediately or not
      CanInsert=gen_statem:call(Pid, {insert,From,Key,Value,C}),
      case CanInsert of
        true->{keep_state,{OriginalCapcity,NewC,[{Key,C,Pid}|NewList]}};
        false->keep_state_and_data
      end
  end;
server({call,From},{update,Key,Value,C},{OriginalCapcity, CurrentCapcity, List})->
  case schedule(Key,From,C,OriginalCapcity,CurrentCapcity,List) of
    {exceed,[]}->keep_state_and_data;
    {scheduled1,_NewC,_NewList}-> 
      gen_statem:reply(From,{error,notFindKey}),
      keep_state_and_data;
    {scheduled2,NewC,NewList,{_Key1,_OldC,Pid}}->
      % Here is to Judge whether can insert immediately or not
      CanUpsert=gen_statem:call(Pid, {upsert,From,Key,Value,C}),
      case CanUpsert of
        true->{keep_state,{OriginalCapcity,NewC,[{Key,C,Pid}|NewList]}};
        false->keep_state_and_data
      end
  end;
server({call,From},all_items,{_OriginalCapcity, _CurrentCapcity, List})->
  AllItemList=lists:map(fun({K,C,Pid})->{K,gen_statem:call(Pid,read),C}end,List),
  gen_statem:reply(From, AllItemList),
  keep_state_and_data;
server({call,From},stop,{_OriginalCapcity, _CurrentCapcity, List})->
  lists:map(fun({_K,_C,P})->gen_statem:call(P,stop)end,List),
  gen_statem:reply(From, ok),
  stop.
schedule(Key,From,C,OriginalCapcity, CurrentCapcity, List)->
  case C>OriginalCapcity of
    true->gen_statem:reply(From,{error,capcityNotEnough}),{exceed,[]};
    false->case spiltItem(Key, List) of
      {nothing,_List}->
        % None exists Key 
        {NewC,NewList}=removeLRU(C, CurrentCapcity, lists:reverse(List)),{scheduled1,NewC,NewList};
      {{_,OldC,Pid},NewList}->{NewC,NewList}=removeLRU(C, OldC+CurrentCapcity, lists:reverse(NewList)),
      {scheduled2,NewC,NewList,{Key,OldC,Pid}}
    end
end.
spiltItem(Key,List)-> {findItem(Key, List),deleteItem(Key, List)}.
deleteItem(_Key,[])->[];
deleteItem(Key,[{Key1,C1,Pid}|Rest])->
  if Key==Key1 ->Rest;true->deleteItem(Key,[{Key1,C1,Pid}|deleteItem(Key,Rest)])
end.
findItem(_Key,[])->nothing;
findItem(Key,[{Key1,C1,Pid}|Rest])->
  if Key1==Key
   ->{find,{Key1,C1,Pid}}; true->findItem(Key,Rest) 
  end.
removeLRU(C,NewC,List)->
  case C <NewC of
    true->
      case List of
        []->throw ("Unknown error");
        [{_Key,C1,Pid}|Rest]->gen_statem:call(Pid, stop),removeLRU(C+C1, NewC, Rest)
      end;
    false->{NewC,List}
  end.
findPid(List,Key)->
  case List of
    []->nothing;
    [{Key1,_C1,Pid1}|Rest]-> 
      if Key1==Key ->{ok,Pid1};true->findPid(Rest,Key)end
  end.
callback_mode()->state_functions.
init({server,Capcity}) ->
  {ok, server,{Capcity, Capcity, []}};
init({cache,Key,Value,Capcity})->
  {ok,normal,{Key,Value,Capcity,[]}}.

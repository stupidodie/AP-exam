-module(frappe).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called frappe.

% Export at least the API:
-export([fresh/1, set/4, read/2, insert/4, update/4, upsert/3, stable/3, all_items/1,
         stop/1]).

-behaviour(gen_statem).
% You may have other exports as well
-export([init/1,server/3]).
-export([normal/3,upsertState/3,emptyState/3]).
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
% In this state, it can be update, read, set but not insert
normal(cast,{set,_Key1,Value1,C1},{ServerId,OriginalCapcity,Key,_Value,_Capcity,_WaitList})->
  {keep_state,{ServerId,OriginalCapcity,Key,Value1,C1,[]}};
normal({call,From},read,{_ServerId,_OriginalCapcity,_Key,Value,Capcity,_WaitList})->
  case Capcity of
    0-> gen_statem:reply(From,nothing),keep_state_and_data;
    _->gen_statem:reply(From,Value),keep_state_and_data
  end;
normal({call,From},{insert,From1,Key1,Value,_C},{ServerId,OriginalCapcity,_Key,Value,Capcity,WaitList})->
  case Capcity of
    0->gen_statem:reply(From,true),
      gen_statem:reply(From1,ok),
      {keep_state,{ServerId,OriginalCapcity,Key1,Value,Capcity,WaitList}};
    _->gen_statem:reply(From,false),gen_statem:reply(From1,{error,existKey}),
      keep_state_and_data
  end;
normal(cast,{insert,From1,Key1,Value,_C},{ServerId,OriginalCapcity,_Key,Value,Capcity,WaitList})->
  case Capcity of
    0->  gen_statem:reply(From1,ok),
      {keep_state,{ServerId,OriginalCapcity,Key1,Value,Capcity,WaitList}};
    _->gen_statem:reply(From1,{error,existKey}),
      keep_state_and_data
  end;
normal({call,From},{update,From1,Key1,Value1,C1},{ServerId,OriginalCapcity,_Key,_Value,Capcity,WaitList})->
  case Capcity of
    0-> gen_statem:reply(From, false),
    gen_statem:reply(From1, {error,noneKey}),keep_state_and_data;
  _->gen_statem:reply(From, true),
  gen_statem:reply(From1, ok),
  {keep_state,{ServerId,OriginalCapcity,Key1,Value1,C1,WaitList}}
  end;
normal(cast,{update,From1,Key1,Value1,C1},{ServerId,OriginalCapcity,_Key,_Value,Capcity,WaitList})->
  case Capcity of
    0-> gen_statem:reply(From1, {error,noneKey}),keep_state_and_data;
  _->gen_statem:reply(From1, ok),
  {keep_state,{ServerId,OriginalCapcity,Key1,Value1,C1,WaitList}}
  end;
normal(cast,{upsert,From,Fun,Arg},{ServerId,OriginalCapcity,Key,Value,Capcity,WaitList})->
  worker(self(), Fun, Arg),
  {next_state,upsertState,{ServerId,From,OriginalCapcity,Key,Value,Capcity,WaitList}};
normal(cast,stop,_)->stop;
normal(cast,_,_)->
  keep_state_and_data.
upsertState(cast,{set,_Key1,Value1,C1},{ServerId,_From1,OriginalCapcity,Key,_Value,_Capcity,_WaitList})->
  {next_state,normal,{ServerId,OriginalCapcity,Key,Value1,C1,[]}};
upsertState({call,From},read,{_ServerId,_From1,_OriginalCapcity,_Key,Value,Capcity,_WaitList})->
  case Capcity of
    0-> gen_statem:reply(From,nothing),keep_state_and_data;
    _->gen_statem:reply(From,Value),keep_state_and_data
  end;
upsertState({call,From},{insert,From1,Key1,Value1,C},{ServerId,From2,OriginalCapcity,Key,Value,Capcity,WaitList})->
  gen_statem:reply(From, false),
  {keep_state,{ServerId,From2,OriginalCapcity,Key,Value,Capcity,[{insert,From1,Key1,Value1,C}|WaitList]}};
upsertState({call,From},{update,From1,Key1,Value1,C1},{ServerId,From2,OriginalCapcity,Key,Value,Capcity,WaitList})->
  gen_statem:reply(From, false),
  {keep_state,{ServerId,From2,OriginalCapcity,Key,Value,Capcity,[{update,From1,Key1,Value1,C1}|WaitList]}};
upsertState(cast,{upsert,From1,Fun1,Arg1},{ServerId,From2,OriginalCapcity,Key,Value,Capcity,WaitList})->
  {keep_state,{ServerId,OriginalCapcity,From2,Key,Value,Capcity,[{upsert,From1,Fun1,Arg1}|WaitList]}};
upsertState(cast,stop,_)->stop;
upsertState(cast,{new_value,Val,C},{ServerId,From2,OriginalCapcity,Key,Value,Capcity,WaitList})->
  case C>OriginalCapcity of
    true->gen_statem:reply(From2, {error,notEnoughCapcity}),
      case Capcity of 
      0->recover(ServerId,WaitList),{next_state,emptyState,{ServerId,OriginalCapcity,Key}};
      _->recover(ServerId,WaitList),{next_state,normal,{ServerId,OriginalCapcity,Key,Value,Capcity,[]}}
    end;
  false-> case Capcity of
    0->recover(ServerId,WaitList),gen_statem:reply(From2,ok),{next_state,emptyState,{ServerId,OriginalCapcity,Key}};
    _->recover(ServerId,WaitList),gen_statem:reply(From2,ok),{next_state,normal,{ServerId,OriginalCapcity,Key,Val,C,[]}}
  end
  end;
upsertState(cast,unchange,{ServerId,From2,OriginalCapcity,Key,Value,Capcity,WaitList})->
  case Capcity of
    0->recover(ServerId,WaitList),gen_statem:reply(From2,ok),{next_state,emptyState,{ServerId,OriginalCapcity,Key}};
    _->recover(ServerId,WaitList),gen_statem:reply(From2,ok),{next_state,normal,{ServerId,OriginalCapcity,Key,Value,Capcity,[]}}
  end;
upsertState(cast,_,_)->
  keep_state_and_data.
emptyState(cast,{set,Key1,Value1,C1},{ServerId,OriginalCapcity,_Key})->
  {next_state,normal,{ServerId,OriginalCapcity,Key1,Value1,C1,[]}};
emptyState({call,From},read,{_ServerId,_OriginalCapcity})->
  gen_statem:reply(From, nothing),
 keep_state_and_data;
emptyState({call,From},{insert,From1,Key1,Value,C},{ServerId,OriginalCapcity,_Key})->
  gen_statem:reply(From, true),
  gen_statem:reply(From1,ok),
  {next_state,normal,{ServerId,OriginalCapcity,Key1,Value,C,[]}};
emptyState({call,From},{update,From1,_Key1,_Value1,_C1},{_ServerId,_OriginalCapcity,_Key})->
  gen_statem:reply(From, false),
  gen_statem:reply(From1, {error,noneKey}),keep_state_and_data;
emptyState(cast,{upsert,From,Fun,Arg},{ServerId,OriginalCapcity,Key})->
  worker(self(), Fun, Arg),
  {next_state,upsertState,{ServerId,From,OriginalCapcity,Key,0,0,[]}}.


recover(PId,List)->
  spawn(
    fun()->lists:map(fun(Operation)->
      case Operation of
        {insert,From1,Key1,Value1,C}->gen_statem:cast(PId,{insert,From1,Key1,Value1,C});
        {update,From1,Key1,Value1,C1}->gen_statem:cast(PId,{update,From1,Key1,Value1,C1});
        {upsert,From1,Fun1,Arg1}->gen_statem:cast(PId,{upsert,From1,Fun1,Arg1})
      end
     end, lists:reverse(List))
    end
  ).

worker(Pid,Fun,Arg)->
  spawn(fun()->
    try Result=Fun(Arg),
        case Result of 
        {new_value, Val, C}->gen_statem:cast(Pid, {new_value, Val, C});
        _->gen_statem:cast(Pid, unchange)
      end
    catch throw:Term->
      case Term of
        {new_value, Val1, C1}->gen_statem:cast(Pid, {new_value, Val1, C1});
        _->gen_statem:cast(Pid, unchange)
      end;
    _:_->gen_statem:cast(Pid, unchange)
    end
  end).
server({call,From},{set, Key, Value, C},{OriginalCapcity, CurrentCapcity, List})->
  case schedule(Key,From,C,OriginalCapcity,CurrentCapcity,List) of
    {exceed,[]}->keep_state_and_data;
    {scheduled1,NewC,NewList}->
      {ok,Pid}=gen_statem:start(?MODULE, {self(),cache,OriginalCapcity,Key,Value,C}, []),
      gen_statem:reply(From,ok),
      {keep_state,{OriginalCapcity,NewC,[{Key,C,Pid}|NewList]}};
    {scheduled2,NewC,NewList,{_Key1,_OldC,Pid}}->
      gen_statem:cast(Pid,{set,Key,Value,C}),gen_statem:reply(From,ok),
      {keep_state,{OriginalCapcity,NewC,[{Key,C,Pid}|NewList]}}
  end;
server({call,From},{read,Key},{_OriginalCapcity, _CurrentCapcity, List})->
  case findPid(Key, List) of
    nothing->gen_statem:reply(From, nothing),keep_state_and_data;
    {ok,Pid}->Result=gen_statem:call(Pid, read),
    case Result of
       nothing->gen_statem:reply(From,nothing);
      _->gen_statem:reply(From, {ok,Result}) 
    end,
     keep_state_and_data
  end;
server({call,From},{insert,Key,Value,C},{OriginalCapcity, CurrentCapcity, List})->
  case schedule(Key,From,C,OriginalCapcity,CurrentCapcity,List) of
    {exceed,[]}->keep_state_and_data;
    {scheduled1,NewC,NewList}-> 
      {ok,Pid}=gen_statem:start(?MODULE, {self(),cache,OriginalCapcity,Key,Value,C}, []),
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
server(cast,{insert,From,Key,Value,C},{OriginalCapcity, CurrentCapcity, List})->
  case schedule(Key,From,C,OriginalCapcity,CurrentCapcity,List) of
    {exceed,[]}->keep_state_and_data;
    {scheduled1,NewC,NewList}-> 
      {ok,Pid}=gen_statem:start(?MODULE, {self(),cache,OriginalCapcity,Key,Value,C}, []),
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
      CanUpdate=gen_statem:call(Pid, {update,From,Key,Value,C}),
      case CanUpdate of
        true->{keep_state,{OriginalCapcity,NewC,[{Key,C,Pid}|NewList]}};
        false->keep_state_and_data
      end
  end;
server(cast,{update,From,Key,Value,C},{OriginalCapcity, CurrentCapcity, List})->
  case schedule(Key,From,C,OriginalCapcity,CurrentCapcity,List) of
    {exceed,[]}->keep_state_and_data;
    {scheduled1,_NewC,_NewList}-> 
      gen_statem:reply(From,{error,notFindKey}),
      keep_state_and_data;
    {scheduled2,NewC,NewList,{_Key1,_OldC,Pid}}->
      % Here is to Judge whether can insert immediately or not
      CanUpdate=gen_statem:call(Pid, {update,From,Key,Value,C}),
      case CanUpdate of
        true->{keep_state,{OriginalCapcity,NewC,[{Key,C,Pid}|NewList]}};
        false->keep_state_and_data
      end
  end;
server({call,From},{upsert,Key,Fun},{OriginalCapcity, CurrentCapcity, List})->
  case findItem(List, Key) of
    nothing->
      {ok,Pid}=gen_statem:start(?MODULE, {self(),cache,OriginalCapcity,Key,0,0}, []),
      gen_statem:cast(Pid,{upsertState,From,Fun,new}),
      {keep_state,{OriginalCapcity, CurrentCapcity, [{Key,0,Pid}|List]}};
    {_,C1,Pid}->gen_statem:cast(Pid,{upsertState,From,Fun,{existing,C1}}),
     keep_state_and_data
      
  end;
server(cast,{upsert,From,Key,Fun},{OriginalCapcity, CurrentCapcity, List})->
  case findItem(List, Key) of
    nothing->
      {ok,Pid}=gen_statem:start(?MODULE, {self(),cache,OriginalCapcity,Key,0,0}, []),
      gen_statem:cast(Pid,{upsertState,From,Fun,new}),
      {keep_state,{OriginalCapcity, CurrentCapcity, [{Key,0,Pid}|List]}};
    {_,C1,Pid}->gen_statem:cast(Pid,{upsertState,From,Fun,{existing,C1}}),
     keep_state_and_data
  end;
server({call,From},all_items,{_OriginalCapcity, _CurrentCapcity, List})->
  AllItemList=lists:map(fun({K,C,Pid})->case gen_statem:call(Pid,read) of nothing->{K,0,0};Result->{K,Result,C} end end,List),
  ReturnList=filterList(AllItemList),
  gen_statem:reply(From, ReturnList),
  keep_state_and_data;
server({call,From},stop,{_OriginalCapcity, _CurrentCapcity, List})->
  lists:map(fun({_K,_C,P})->gen_statem:cast(P,stop)end,List),
  gen_statem:reply(From, ok),
  stop.
schedule(Key,From,C,OriginalCapcity, CurrentCapcity, List)->
  case C>OriginalCapcity of
    true->gen_statem:reply(From,{error,capcityNotEnough}),{exceed,[]};
    false->case spiltItem(Key, List) of
      {nothing,_List}->
        % None exists Key 
        {NewC,NewList}=removeLRU(C, CurrentCapcity, lists:reverse(List)),{scheduled1,NewC,NewList};
      {{_,OldC,Pid},NewList}->
        {NewC,NewList}=removeLRU(C, OldC+CurrentCapcity, lists:reverse(NewList)),
      {scheduled2,NewC,NewList,{Key,OldC,Pid}}
    end
end.
filterList(List)->
  case List of 
  []->[];
  [{K,V,C}|Rest]->if C==0 ->Rest ;true->[{K,V,C}|filterList(Rest)] end
end.
spiltItem(Key,List)-> {findItem(Key, List),deleteItem(Key, List)}.
deleteItem(_Key,[])->[];
deleteItem(Key,[{Key1,C1,Pid}|Rest])->
  if Key==Key1 ->Rest;true->deleteItem(Key,[{Key1,C1,Pid}|deleteItem(Key,Rest)])
end.
findItem(_Key,[])->nothing;
findItem(Key,[{Key1,C1,Pid}|Rest])->
  if Key1==Key
   ->{Key1,C1,Pid}; true->findItem(Key,Rest) 
  end.
removeLRU(C,NewC,List)->
  case C <NewC of
    true->
      case List of
        []->throw ("Unknown error");
        [{_Key,C1,Pid}|Rest]->if C1>0 ->gen_statem:call(Pid, stop),removeLRU(C+C1, NewC, Rest);
      true->removeLRU(C,NewC,Rest)
      end
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
init({ServerId,cache,OriginalCapcity,Key,Value,Capcity})->
  {ok,normal,{ServerId,OriginalCapcity,Key,Value,Capcity,[]}}.

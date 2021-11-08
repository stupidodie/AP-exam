-module(frappe).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called frappe.

% Export at least the API:
-export([fresh/1,
         set/4,
         read/2,
         insert/4,
         update/4,
         upsert/3,
         stable/3,
         all_items/1,
         stop/1
        ]).
-behaviour(gen_server).
% You may have other exports as well
-export([handle_call/3,init/1,handle_cast/2]).



fresh(Capcity) -> gen_server:start(?MODULE, Capcity, []).

set(FS, Key, Value, C) ->gen_server:call(FS,{set,Key,Value,C}).

read(FS, Key) ->gen_server:call(FS,{read,Key}).

insert(FS, Key, Value, C) ->gen_server:call(FS,{insert,Key,Value,C}).

update(FS, Key, Value, C) ->gen_server:call(FS,{update,Key,Value,C}).

upsert(FS, Key, Fun) ->gen_server:call(FS,{upsert,Key,Fun}).

stable(FS, Key, Ref) ->gen_server:call(FS,{stable,Key,Ref}).

all_items(FS) ->gen_server:call(FS,all_items).

stop(FS) ->gen_server:call(FS,stop).

handle_call({set,Key,Value,C}, _From,{OriginalCapcity,CurrentCapcity,List}) ->
  case C =< OriginalCapcity of
    true ->
      case C=<CurrentCapcity of
        true->
          case searchItem(List, Key) of
            true->
              {_,_,C1,_,_,_,_,_}=findItem(List, Key),
              NewList=deleteItem(List, Key),
              {reply,ok,{OriginalCapcity,CurrentCapcity-C+C1,[{Key,Value,C,false,[],false,[],[]}|NewList]}};
            false->{reply,ok,{OriginalCapcity,CurrentCapcity-C,[{Key,Value,C,false,[],false,[],[]}|List]}}
          end;
        false->
          {NewCapcity,NewList}=releaseItem(C,CurrentCapcity,List),
          {reply,ok,{OriginalCapcity,NewCapcity-C,[{Key,Value,C,false,[],false,[],[]}|NewList]}}
      end;
    false ->
      {reply,{error,"invalid cost"},{OriginalCapcity,CurrentCapcity,List}}
  end;
handle_call({read,Key},_From,{OriginalCapcity,CurrentCapcity,List})->
  case searchItem(List, Key) of
    true->
      {Key1,Value1,C1,Judge1,Wait1,Judge2,R1,S1}=findItem(List, Key),
      case C1==0 of
        false->
          NewList=deleteItem(List, Key),
          {reply,{ok,Value1},{OriginalCapcity,CurrentCapcity,[{Key1,Value1,C1,Judge1,Wait1,Judge2,R1,S1}|NewList]}};
        true->{reply,nothing,{OriginalCapcity,CurrentCapcity,List}}
      end;
    false->{reply,nothing,{OriginalCapcity,CurrentCapcity,List}}
  end;
handle_call({insert,Key,Value,C},From,{OriginalCapcity,CurrentCapcity,List})->
  case searchItem(List, Key) of
    true->
      {_,_,C1,Judge1,_,Judge2,_,_}=findItem(List, Key),
      case C1==0 of
        false->
      case  Judge1 of
        false->
          case Judge2 of
            false->{reply,{error,alreadyExists},{OriginalCapcity,CurrentCapcity,List}};
            true->NewList=addRecovery(Key,From,{insert,Key,Value,C},List),
              {noreply,{OriginalCapcity,CurrentCapcity,NewList}}
        end;
        true->
          NewList=addWait(Key, From, {insert,Key,Value,C}, List),
          {noreply,{OriginalCapcity,CurrentCapcity,NewList}}
      end;
    true->case C>OriginalCapcity of
      true-> {reply,{error, invalidCost},{OriginalCapcity,CurrentCapcity,List}};
      false->
        case C =<CurrentCapcity of
          true->{reply,ok,{OriginalCapcity,CurrentCapcity-C,[{Key,Value,C,false,[],false,[],[]}|List]}};
          false->
            {NewCapcity,NewList}=releaseItem(C,CurrentCapcity,List),
            {reply,ok,{OriginalCapcity,NewCapcity-C,[{Key,Value,C,false,[],false,[],[]}|NewList]}}
        end
    end
    end;
    false->
      case C>OriginalCapcity of
        true-> {reply,{error, invalidCost},{OriginalCapcity,CurrentCapcity,List}};
        false->
          case C =<CurrentCapcity of
            true->{reply,ok,{OriginalCapcity,CurrentCapcity-C,[{Key,Value,C,false,[],false,[],[]}|List]}};
            false->
              {NewCapcity,NewList}=releaseItem(C,CurrentCapcity,List),
              {reply,ok,{OriginalCapcity,NewCapcity-C,[{Key,Value,C,false,[],false,[],[]}|NewList]}}
          end
      end
  end;
handle_call({update,Key,Value,C},From,{OriginalCapcity,CurrentCapcity,List})->
  case searchItem(List, Key) of
    false->{reply,{error, notFindKey},{OriginalCapcity,CurrentCapcity,List}};
    true->  
      case C>OriginalCapcity of
        true-> {reply,{error, invalidCost},{OriginalCapcity,CurrentCapcity,List}};
        false->{_,_,C1,Judge1,_,Judge2,_,S1}=findItem(List, Key),
          case C1 ==0 of
            false->
              case  Judge1 of
                %Just Update
              false->
                case Judge2 of 
                  false->ListAfterDelete=deleteItem(List, Key),
                    case C=< CurrentCapcity+C1 of
                      true->{reply,ok,{OriginalCapcity,CurrentCapcity+C1-C,[{Key,Value,C,false,[],false,[],[]}|ListAfterDelete]}};
                      false->
                      {NewCapcity,NewList}=releaseItem(C,CurrentCapcity+C1,ListAfterDelete),
                    { reply,ok,{OriginalCapcity,NewCapcity-C,[{Key,Value,C,false,[],false,[],S1}|NewList]}}
                    end;
                  true->NewList=addRecovery(Key,From,{update,Key,Value,C},List),
                  {noreply,{OriginalCapcity,CurrentCapcity,NewList}}
                  end;
          % while the Key is occupied....
              true->
                NewList=addWait(Key, From,{update,Key,Value,C}, List),
                {noreply,{OriginalCapcity,CurrentCapcity,NewList}}
              end;
              true->{reply,{error, notFindKey},{OriginalCapcity,CurrentCapcity,List}}
      end
    end
  end;
handle_call({upsert,Key,Fun},From,{OriginalCapcity,CurrentCapcity,List})->
  case searchItem(List, Key) of
    true->{_,Value1,_,Judge1,_,Judge2,_,_}=findItem(List, Key),
          case Judge1==false of
            true-> 
            case Judge2 of
              false->handle_upsert_fun(Fun, {existing,Value1}, Key,From,self()),
                  {noreply,{OriginalCapcity,CurrentCapcity,setJudge1(Key, List,true)}};
              true->NewList=addRecovery(Key,From,{upsert,Key,Fun},List),
              {noreply,{OriginalCapcity,CurrentCapcity,NewList}}
            end;
            false->
              NewList=addWait(Key, From, {upsert,Key,Fun}, List),
              {noreply,{OriginalCapcity,CurrentCapcity,NewList}}
          end;
    false->
      handle_upsert_fun(Fun,new,Key, From,self()),
      {noreply,{OriginalCapcity,CurrentCapcity,[{Key,[],0,true,[],false,[],[]}|List]}}
  end;
handle_call({stable,Key,Ref},From,{OriginalCapcity,CurrentCapcity,List})->
  case searchItem(List, Key) of
    true->
  {_,Val,C,Judge1,W1,Judge2,R1,S1}=findItem(List, Key),
  case C==0 of
    false->
  case (Judge1==false) and  (Judge2==false) of
    true->NewList=deleteItem(List, Key),{reply,{Ref,Val},{OriginalCapcity,CurrentCapcity,[{Key,Val,C,Judge1,W1,Judge2,R1,S1}|NewList]}};
    false->{noreply,addStable(Key,From,{Key,Ref},List)}
    end;
  true->{reply,{error,nokey},{OriginalCapcity,CurrentCapcity,List}}
  end;
  false->{reply,{error,nokey},{OriginalCapcity,CurrentCapcity,List}}
  end;
handle_call(all_items,_From,{OriginalCapcity,CurrentCapcity,List})->
  {reply,lists:map(fun({Key,Value,C,_,_,_,_,_})->{Key,Value,C} end, List),{OriginalCapcity,CurrentCapcity,List}};
handle_call(stop,From,{_,_,_List})->
  % lists:foreach(fun({_,_,_,_,W1,_,R1,S1})->sendStopMessage(W1),sendStopMessage(R1),sendStopMessage(S1) end, List),
  gen_server:reply(From, ok),
  gen_server:stop(self()).
  
handle_cast({recoverJudge,Key,Option1,Option2},{OriginalCapcity,CurrentCapcity,List})-> 
    {_,_,_,Judge1,_,_,_,_}=findItem(List, Key),
  % if it 's false means it's been reset, then set it to the true
  case Judge1 of 
    true->{noreply,{OriginalCapcity,CurrentCapcity,setJudge2(Key,setJudge1(Key, List, Option1),Option2)}};
    false->{noreply,{OriginalCapcity,CurrentCapcity,setJudge2(Key,setJudge1(Key, List, true),Option2)}}
  end;
handle_cast({afterRecover,Key,Option},{OriginalCapcity,CurrentCapcity,List})->
    case searchItem(List, Key) of
      true->
        {_,Value,C,Judge1,W1,Judge2,R1,S1}=findItem(List, Key),
        case Judge1 of
          false->
            case R1==[] of
              false->recover(R1),gen_server:cast(self(),{afterRecover,Key,Option});
              true->NewList=deleteItem(List, Key),lists:foreach(fun({From,{_,Ref}})->gen_server:reply(From, {Ref,Value})end, S1),{noreply,{OriginalCapcity,CurrentCapcity,setJudge2(Key,[{Key,Value,C,Judge1,W1,Judge2,R1,[]}|NewList],Option)}}
              end;
            true->{noreply,{OriginalCapcity,CurrentCapcity,setJudge1(Key,[{Key,Value,C,false,W1,Judge2,R1,S1}|List],false)}}
           end;
        false->{noreply,{OriginalCapcity,CurrentCapcity,List}}
  end;
handle_cast({upsert,From,Type,Key},{OriginalCapcity,CurrentCapcity,List})->
  case Type of 
  unchange-> 
      {_,_,C1,J1,W1,_,_,_}=findItem(List, Key),
      case C1==0 of 
        false->
          case J1 of 
          false-> recover(W1),gen_server:reply(From, ok),{noreply,{OriginalCapcity,CurrentCapcity,List}};
          true->{noreply,{OriginalCapcity,CurrentCapcity,List}}
          end;
        true->NewList=deleteItem(List, Key),gen_server:reply(From, ok),{noreply,{OriginalCapcity,CurrentCapcity,NewList}}
      end;
  {new_value,Val,C}->
    case C>OriginalCapcity of
      true->gen_server:reply(From,{error, invalidCost}),{noreply,{OriginalCapcity,CurrentCapcity,List}};
      false->
        case searchItem(List, Key) of
          false->
            case C =< CurrentCapcity of
              true->gen_server:reply(From, ok),{noreply,{OriginalCapcity,CurrentCapcity-C,[{Key,Val,C,false,[],false,[],[]}|List]}};
              false->
                {NewCapcity,NewList}=releaseItem(C,CurrentCapcity,List),gen_server:reply(From, ok),
                {noreply,{OriginalCapcity,NewCapcity-C,[{Key,Val,C,false,[],false,[],[]}|NewList]}}
            end;
          true-> {_,_,C1,J1,W1,J2,R1,S1}=findItem(List, Key),
          case J1 of 
          false->
          ListAfterDelete=deleteItem(List, Key),
            case C =< CurrentCapcity+C1 of
              true->gen_server:reply(From, ok),{noreply,{OriginalCapcity,CurrentCapcity+C1-C,[{Key,Val,C,J1,W1,J2,R1,S1}|ListAfterDelete]}};
              false->{NewCapcity,NewList}=releaseItem(C,CurrentCapcity+C1,ListAfterDelete),
              {noreply,ok,{OriginalCapcity,NewCapcity-C,[{Key,Val,C,false,[],false,[],S1}|NewList]}}
            end;
          true->{noreply,{OriginalCapcity,CurrentCapcity,List}}
          end
        end
    end
  end.

handle_upsert_fun(Fun,Arg,Key,From,Me)->
  spawn_link(
    fun()->
      try Result=Fun(Arg),
      gen_server:cast(Me, {recoverJudge,Key,false,true}),
      case Result of
        {new_value, Val, C}->gen_server:cast(Me,{upsert,From,{new_value,Val,C},Key}),gen_server:cast(Me,{afterRecover,Key,false});
        _->gen_server:cast(Me,{upsert,From,unchange,Key}),gen_server:cast(Me,{afterRecover,Key,false})
      end
      catch throw:Term-> 
        gen_server:cast(Me, {recoverJudge,Key,false,true}),
        case Term of
          {new_value, Val1, C1}->gen_server:cast(Me,{upsert,From,{new_value,Val1,C1},Key}),gen_server:cast(Me,{afterRecover,Key,false});
          _->gen_server:cast(Me,{upsert,From,unchange,Key}),gen_server:cast(Me,{afterRecover,Key,false})
        end;
      error:_-> gen_server:cast(Me, {recoverJudge,Key,false,true}),gen_server:cast(Me,{upsert,From,unchange,Key}),gen_server:cast(Me,{afterRecover,Key,false});
      exit:_->gen_server:cast(Me, {recoverJudge,Key,false,true}),gen_server:cast(Me,{upsert,From,unchange,Key}),gen_server:cast(Me,{afterRecover,Key,false})
      end
    end
  ).
addRecovery(Key,From,Operation,List)->
  case List of
    []->[];
    [{Key1,Value1,C1,Judge1,Wait1,Judge2,R1,S1}|Rest]->case Key1==Key of
      true->[{Key1,Value1,C1,Judge1,Wait1,Judge2,[{From,Operation}|R1],S1}|Rest];
      false->[{Key1,Value1,C1,Judge1,Wait1,Judge2,R1,S1}|addRecovery(Key,From,Operation, Rest)]
    end
  end.

% Maybe the process1 slower than 2?
recover(Wait)->
  lists:foreach(
    fun({From,Operation})->spawn(fun()->
      Result=gen_server:call(self(),Operation),
      gen_server:reply(From, Result)
  end)end, lists:reverse(Wait)).
setJudge1(Key,List,Option)->
  case List of
    []->[];
    [{Key1,V1,C1,Judge,W1,Judge2,R1,S1}|Rest]-> case Key1==Key of true-> [{Key1,V1,C1,Option,W1,Judge2,R1,S1}|Rest];false->[{Key1,V1,C1,Judge,W1,Judge2,R1,S1}|setJudge1(Key,Rest,Option)]end
  end.
setJudge2(Key, List, Option)->
  case List of
    []->[];
    [{Key1,V1,C1,Judge1,W1,Judge2,R1,S1}|Rest]-> case Key1==Key of true-> [{Key1,V1,C1,Judge1,W1,Option,R1,S1}|Rest];false->[{Key1,V1,C1,Judge1,W1,Judge2,R1,S1}|setJudge1(Key,Rest,Option)]end
  end.

% For the wait operation, we use the Last for the Oldest wait 
addWait(Key,From,Operation,List)->
  case List of 
    []->[];
    [{Key1,Value1,C1,Judge1,Wait1,Judge2,R1,S1}|Rest]->
      case Key1==Key of
        true->[{Key1,Value1,C1,Judge1,[{From,Operation}|Wait1],Judge2,R1,S1}|Rest];
        false->[{Key1,Value1,C1,Judge1,Wait1,Judge2,R1,S1}|addWait(Key,From,Operation,Rest)]
      end
  end.
addStable(Key,From,Operation,List)->
  case List of 
    []->[];
    [{Key1,Value1,C1,Judge1,Wait1,Judge2,R1,S1}|Rest]->
      case Key1==Key of
        true->[{Key1,Value1,C1,Judge1,Wait1,Judge2,R1,[{From,Operation}|S1]}|Rest];
        false->[{Key1,Value1,C1,Judge1,Wait1,Judge2,R1,S1}|addStable(Key,From,Operation,Rest)]
      end
  end.
% I choose the Just ignore way
% Which means when need sapce for item 
% Although some item has ongoing operations
% but if it's the Oldest, just remove
releaseItem(C,CurrentCapcity,List)->
  case C=< CurrentCapcity of
    true->{CurrentCapcity,List};
    false->
      case List of
        []->throw("Error,not enough sapce");
        _->
          {_,_,C1,_,Wait1,_,_,_}=lists:last(List),
          sendRemovedMessage(Wait1),
          releaseItem(C, CurrentCapcity+C1, lists:droplast(List))
      end
  end.
sendRemovedMessage(Wait)->
  case Wait of
    []->[];
    [{From,_}|Rest]->gen_server:reply(From, {error,removed}),sendRemovedMessage(Rest)
  end.
% sendStopMessage(List)->
%   case List of
%     []->[];
%     [{From,_}|Rest]->gen_server:reply(From, {error,stopped}),sendStopMessage(Rest)
%   end.
findItem(List,Key)->
   case List of
    [] ->[];
    [{Key1,Value1,C1,Judge1,Wait1,Judge2,R1,S1}|Rest] ->
      case Key1 =:= Key of
        true ->
          {Key1,Value1,C1,Judge1,Wait1,Judge2,R1,S1};
        false ->
          findItem(Rest,Key)
      end
    end. 
deleteItem(List,Key)->
  case List of
    []->[];
    [{Key1,Value1,C1,Judge1,Wait1,Judge2,R1,S1}|Rest]->
      case Key1=:= Key of
        true->Rest;
        false->[{Key1,Value1,C1,Judge1,Wait1,Judge2,R1,S1}|deleteItem(Rest, Key)]
  end
  end.
searchItem(List,Key)->
  case List of
    [] ->
      false;
    [{Key1,_,_,_,_,_,_,_}|Rest] ->
      case Key1 =:= Key of
        true ->
          true;
        false ->
          searchItem(Rest,Key)
      end
    end.
init(Capcity)->{ok,{Capcity,Capcity,[]}}.
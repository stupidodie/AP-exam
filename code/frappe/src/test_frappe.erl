-module(test_frappe).

-export([test_all/0, test_everything/0]).
-export([mktrans/2,terminating_transformation/1]). % Remember to export the other functions from Q2.2
-include_lib("eqc/include/eqc.hrl").
-include("apqc_statem.hrl").
-behaviour(apqc_statem).
-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).
-export([prop_cache_under_capacity/0,serverstart/1]).
-debug([export_all]).
% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_frappe.
-import(test_eunit_frappe,[test_eunit_test_all/0]).
test_all() ->[test_eunit_frappe:test_eunit_test_all()].
test_everything() ->
  test_all().
opr()->eqc_gen:elements([{throw,new},{throw,unchange},{normal,new},{normal,unchange},loop]).

%% THE SECOND ARGUMENT IS TO ENSURE THAT THE CAPCITY FOR THE KEY
%%  IS NOT GREATER THAN THE CACHE CAPCITY
mktrans({throw,new},Arg)->
 fun(Input)->
  case Input of
  {existing,V1}->throw({new_value,V1+1,Arg});
  new->throw({new_value,Arg,Arg})
  end
end;
mktrans({throw,unchange},_Arg)->
  fun(Input)->
    case Input of
    {existing,_}->throw(unchange);
    new->throw(unchange)
    end
  end;
mktrans({normal,new},Arg)->
  fun(Input)->
    case Input of
    {existing,V1}->{new_value,V1+1,Arg};
    new->{new_value,Arg,Arg}
    end
  end;
mktrans({normal,unchange},_Arg)->
  fun(Input)->
    case Input of
    {existing,_}->unchange;
    new->throwunchange
    end
  end.

terminating_transformation(Gen)->
  ?LET({Opr,Arg},{opr(),eqc_gen:choose(1,Gen)},[call,test_frappe,mktrans,[Opr,Arg]]).

-type frappe_mode():: #{serv:=any(),capcity:=integer(),current:=integer(),keyList:=[{integer(),integer(),integer()}]}.
prop_cache_under_capacity()->
  ?FORALL(Cmds,commands(?MODULE),
  begin
    % eqc:format("~p~n",[Cmds]),
    {_,S,_}=Result=run_commands(?MODULE,Cmds),
    % eqc:format("~p~n",[Result]),
    cleanup(S),
    check_commands(Cmds,Result)
    end).
cleanup(#{serv:=none})->ok;
cleanup(#{serv:=FS})->frappe:stop(FS).
serverstart(Capcity)->{ok,FS}=frappe:fresh(Capcity),FS.
check_commands(Cmds,{_,_,Res}=HSRes)->
  pretty_commands(?MODULE,Cmds,HSRes,
  aggregate(command_names(Cmds),equals(Res,ok))).
capcity()->eqc_gen:choose(50,100).
command(#{serv:=none})->
  return({call,?MODULE,serverstart,[capcity()]});
% command(_S)->oneof([{call,?MODULE,serverstart,[eqc_gen:nat()]}]);
command(#{serv:=FS,capcity:=_})->
  oneof([
      {call,frappe,insert,[FS,eqc_gen:nat(),eqc_gen:char(),eqc_gen:choose(3,20)]}
     ,{call,frappe,update,[FS,eqc_gen:nat(),eqc_gen:char(),eqc_gen:choose(3,20)]}
     ,{call,frappe,set,[FS,eqc_gen:nat(),eqc_gen:char(),eqc_gen:choose(3,20)]}
     ,{call,frappe,read,[FS,eqc_gen:nat()]}
     ,{call,frappe,all_items,[FS]}
  ]).

-spec initial_state()->frappe_mode().
initial_state()->#{serv=>none,capcity=>0,keyList=>[]}.
precondition(_,_)->true.
postcondition(#{serv:=FS},_V,{call,?MODULE,serverstart,_})->
  case FS==none of
    true->false;
    false->true
end;
postcondition(#{keyList:=KeyList,capcity:=Capcity}=_S,V,{call,frappe,set,[_,Key,Value,C]})->
  case V of
    ok->lists:member({Key,Value,C},KeyList);
    _->C>Capcity
  end;
postcondition(#{keyList:=KeyList,capcity:=_Capcity}=_S,V,{call,frappe,update,[_,Key,Value,C]})->
  case V of
    ok->lists:member({Key,Value,C},KeyList);
    _->true
  end;

postcondition(_,_,_)->true.

next_state(S,V,{call,?MODULE,serverstart,[C]})->
  S#{serv:=V,capcity:=C,current=>C};
next_state(#{keyList:=KeyList}=S,V,{call,frappe,set,[_,Key,Value,C]})->
  case V of
    ok->case findItem(Key, KeyList) of 
      nothing->S#{keyList:=[{Key,Value,C}|KeyList]};
      {_V1,_C1}->S#{keyList:=updateItem(Key,Value,C,KeyList)}
  end;
    _->S
end;
next_state(#{keyList:=KeyList}=S,_V,{call,frappe,update,[_,Key,Value,C]})->
  case findItem(Key, KeyList) of
    nothing->S;
    {_V1,_C1}->S#{keyList:=updateItem(Key,Value,C,KeyList)}
  end;
% next_state(#{keyList:=KeyList}=S,_V,{call,frappe,read,[Key]})->
%   case findItem(Key, KeyList) of
%   end;
next_state(S,_,_)->S.
findItem(Key,List)->
  case List of 
    []->nothing;
    [{Key1,V1,C1}|Rest]->case Key1==Key of true->{V1,C1};false->findItem(Key,Rest) end
end.
updateItem(Key,Value,C,List)->
  case List of 
  []->[];
  [{Key1,V1,C1}|Rest]->case Key1==Key of true->[{Key1,Value,C}|Rest];false->[{Key1,V1,C1}|updateItem(Key,Value,C,Rest)] end
end.
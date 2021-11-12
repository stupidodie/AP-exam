-module(test_frappe).

-export([test_all/0, test_everything/0]).
-export([mktrans/2,terminating_transformation/1]). % Remember to export the other functions from Q2.2
-include_lib("eqc/include/eqc.hrl").
-export([prop_cache_under_capacity/0,prop_capacity_invariant/0]).
% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_frappe.
test_all() ->eqc:quickcheck(prop_cache_under_capacity()),eqc:quickcheck(prop_capacity_invariant()),test_eunit_frappe:test_all().
test_everything() ->
  test_all().
opr_no_loop()->eqc_gen:elements([{throw,new},{throw,unchange},{normal,new},{normal,unchange}]).
opr_loop()->loop.
prop_cache_under_capacity()->oneof([
  prop_all_item(),
  prop_read(),
  prop_fresh_and_stop(),
  prop_stable(),
  prop_set_under_capcity(),
  prop_insert_under_capcity(),
  prop_update_under_capcity(),
  prop_upsert_no_loop_under_capcity(),
  prop_upsert_loop()
]).
prop_capacity_invariant()->oneof([
  prop_all_item(),
  prop_read(),
  prop_fresh_and_stop(),
  prop_stable(),
  prop_set_invariant(),
  prop_insert_invariant(),
  prop_update_invariant(),
  prop_upsert_no_loop_invariant(),
  prop_upsert_loop()
]).
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
    new->throw(unchange)
    end
  end;
mktrans(loop,_Arg)->fun(A)-> deadloop(A) end.
deadloop(A)->deadloop(A).
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).
terminating_transformation(Gen)->
  ?LET(Opr,opr_no_loop(),[call,test_frappe,mktrans,[Opr,Gen]]).
none_terminating_transformation(Gen)->
  mktrans(opr_loop(),Gen).
prop_fresh_and_stop()->
  ?FORALL(C,eqc_gen:nat(),
            begin
              eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
              eqc:equals(frappe:stop(FS),ok)
            end).
prop_set_under_capcity()->
    ?FORALL({C,Key,Value},{eqc_gen:nat(),atom_key(),atom_key()},begin
      eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
      eqc:equals(ok,frappe:set(FS, Key, Value, eqc_gen:choose(1,C+5))),
      eqc:equals({ok,Value},frappe:read(FS,Key)),
      eqc:equals(frappe:stop(FS),ok)
      end
    ).
prop_read()->
    ?FORALL({C,Key1,Value1,Key2},{eqc_gen:nat(),atom_key(),atom_key(),atom_key()},begin
      eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
      eqc:equals(ok,frappe:set(FS, Key1, Value1, eqc_gen:choose(1,C+5))),
      eqc:equals(Key2==Key1,case frappe:read(FS, Key2) of  nothing->false;{ok,_}->true end),
      eqc:equals(frappe:stop(FS),ok)
      end
    ).
prop_insert_under_capcity()->
  ?FORALL({C,Key1,Value1,Key2,Value2},{eqc_gen:nat(),atom_key(),atom_key(),atom_key(),atom_key()},begin
    eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
    eqc:equals(ok,frappe:set(FS, Key1, Value1, eqc_gen:choose(1,C+5))),
    eqc:equals(Key2==Key1,case frappe:insert(FS,Key2,Value2,eqc_gen:choose(1,C+5)) of  {error,_}->true;ok->false end),
    case Key2==Key1 of
    true->eqc:equals(frappe:stop(FS),ok);
    false-> eqc:equals({ok,Value2},frappe:read(FS,Key2)),eqc:equals(frappe:stop(FS),ok)
    end
    end
    ).
prop_update_under_capcity()->
  ?FORALL({C,Key1,Value1,Key2,Value2},{eqc_gen:nat(),atom_key(),atom_key(),atom_key(),atom_key()},begin
    eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
    eqc:equals(ok,frappe:set(FS, Key1, Value1, eqc_gen:choose(1,C+5))),
    eqc:equals(Key2==Key1,case frappe:update(FS,Key2,Value2,eqc_gen:choose(1,C+5)) of  {error,_}->false;ok->true end),
    case Key2==Key1 of
      true->eqc:equals({ok,Value2},frappe:read(FS,Key2)),eqc:equals(frappe:stop(FS),ok);
      false-> eqc:equals(frappe:stop(FS),ok)
    end
    end
    ).

prop_upsert_no_loop_under_capcity()->
  ?FORALL({C,Key1,Value1,Key2},{eqc_gen:nat(),atom_key(),atom_key(),atom_key()},begin
    eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
    eqc:equals(ok,frappe:set(FS, Key1, Value1, eqc_gen:choose(1,C+5))),
    Fun=eval(terminating_transformation(C)),
    eqc:equals(ok,frappe:upsert(FS,Key2,Fun)),
    case Key1==Key2 of
    true->
      case calculateResult(Fun,{existing,Value1}) of 
        unchange->eqc:equals({ok,Value1},frappe:read(FS, Key2));
        {new,Value,C}->eqc:equals({ok,Value},frappe:read(FS, Key2))
      end;
    false->
      case calculateResult(Fun,new) of 
        unchange->eqc:equals(nothing,frappe:read(FS, Key2));
        {new,Value,C}->eqc:equals({ok,Value},frappe:read(FS, Key2))
      end
    end,
    eqc:equals(frappe:stop(FS),ok)
    end
  ).
prop_upsert_loop()->
  ?FORALL({C,Key1,Value1,Key2},{eqc_gen:nat(),atom_key(),atom_key(),atom_key()},begin
    eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
    eqc:equals(ok,frappe:set(FS, Key1, Value1, eqc_gen:choose(1,C+5))),
    Fun=eval(none_terminating_transformation(C)),
    eqc:equals(true,
    begin 
      Me=self(),
      spawn_link(fun()->frappe:upsert(FS,Key2,Fun) ,Me!solved end),
      receive
      solved->false,throw(da)
      after 
        20->true
      end 
    end
  )
  end
  ). 
prop_stable()->
  ?FORALL({C,Key1,Value1,Key2},{eqc_gen:nat(),atom_key(),atom_key(),atom_key()},begin
    eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
    eqc:equals(ok,frappe:set(FS, Key1, Value1, eqc_gen:choose(1,C+5))),
    Ref=make_ref(),
    eqc:equals(Key1==Key2,begin frappe:stable(FS,Key2,Ref),  receive {Ref,_}->true;_->false after 50->true end end),
    eqc:equals(frappe:stop(FS),ok)
    end
  ).
prop_all_item()->
  ?FORALL({C,Key1,Value1,Key2,Value2},{eqc_gen:nat(),atom_key(),atom_key(),atom_key(),atom_key()},begin
    eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
    eqc:equals(ok,frappe:set(FS, Key1, Value1, C1=eqc_gen:choose(1,C+5))),
    eqc:equals(ok,frappe:set(FS, Key2, Value2, C2=eqc_gen:choose(1,C+5))),
    case Key1==Key2 of
      true-> eqc:equals([{Key2, Value2, C2}],frappe:all_items(FS));
      false->eqc:equals(lists:sort([{Key1,Value1,C1},{Key2,Value2,C2}]),lists:sort(frappe:all_items(FS)))
    end,
    eqc:equals(frappe:stop(FS),ok)
    end
  ).

prop_set_invariant()->
  ?FORALL({C,Key,Value,Value1},{eqc_gen:nat(),atom_key(),atom_key(),atom_key()},begin
    eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
    % exceed the max capcity
    eqc:equals(false,case frappe:set(FS,Key,Value1,C+20) of ok->true;_->false end),
    eqc:equals(nothing,frappe:read(FS, Key)),
    eqc:equals(ok,frappe:set(FS, Key, Value, eqc_gen:choose(1,C+5))),
    eqc:equals({ok,Value},frappe:read(FS,Key)),
    eqc:equals(frappe:stop(FS),ok)
    end
  ).
prop_insert_invariant()->
  ?FORALL({C,Key1,Value1,Key2,Value2,Value3},{eqc_gen:nat(),atom_key(),atom_key(),atom_key(),atom_key(),atom_key()},begin
    eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
    eqc:equals(ok,frappe:set(FS, Key1, Value1, eqc_gen:choose(1,C+5))),
    eqc:equals(false,case frappe:insert(FS, Key2, Value3, C+20)of ok->true;_->false end),
    eqc:equals(Key2==Key1,case frappe:insert(FS,Key2,Value2,eqc_gen:choose(1,C+5)) of  {error,_}->true;ok->false end),
    case Key2==Key1 of
    true->eqc:equals(frappe:stop(FS),ok);
    false-> eqc:equals({ok,Value2},frappe:read(FS,Key2)),eqc:equals(frappe:stop(FS),ok)
    end
    end
    ).
prop_update_invariant()->
  ?FORALL({C,Key1,Value1,Key2,Value2},{eqc_gen:nat(),atom_key(),atom_key(),atom_key(),atom_key()},begin
    eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
    eqc:equals(ok,frappe:set(FS, Key1, Value1, eqc_gen:choose(1,C+5))),
    eqc:equals(false,case frappe:update(FS, Key1, Value2, C+20) of ok->true; _->false end),
    eqc:equals(Key2==Key1,case frappe:update(FS,Key2,Value2,eqc_gen:choose(1,C+5)) of  {error,_}->false;ok->true end),
    case Key2==Key1 of
      true->eqc:equals({ok,Value2},frappe:read(FS,Key2)),eqc:equals(frappe:stop(FS),ok);
      false-> eqc:equals(frappe:stop(FS),ok)
    end
    end
    ).
calculateResult(Fun,Arg)->
  try Result=Fun(Arg),
  case Result of
    {new_value,V,C}->{new_value,V,C};
    _->unchange
  end
  catch throw:Term->case Term of 
    {new_value,V1,C1}->{new_value,V1,C1};
    _->unchange
  end;
  _:_->unchange
end.
prop_upsert_no_loop_invariant()->
  ?FORALL({C,Key1,Value1,Key2},{eqc_gen:nat(),atom_key(),atom_key(),atom_key()},begin
    eqc:equals(true,case frappe:fresh(C+10) of {ok,FS}->true;_->FS=nothing,false end),
    eqc:equals(ok,frappe:set(FS, Key1, Value1, eqc_gen:choose(1,C+5))),
    Fun1=eval(terminating_transformation(C+20)),
    eqc:equals(false,case frappe:upsert(FS, Key2, Fun1) of ok->true;_->false end),
    Fun=eval(terminating_transformation(C)),
    eqc:equals(ok,frappe:upsert(FS,Key2,Fun)),
    case Key1==Key2 of
    true->
      case calculateResult(Fun,{existing,Value1}) of 
        unchange->eqc:equals({ok,Value1},frappe:read(FS, Key2));
        {new,Value,C}->eqc:equals({ok,Value},frappe:read(FS, Key2))
      end;
    false->
      case calculateResult(Fun,new) of 
        unchange->eqc:equals(nothing,frappe:read(FS, Key2));
        {new,Value,C}->eqc:equals({ok,Value},frappe:read(FS, Key2))
      end
    end,
    eqc:equals(frappe:stop(FS),ok)
    end
  ).
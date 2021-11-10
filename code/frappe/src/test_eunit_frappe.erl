%% Copyright (c) 2021 GuanRan Tai
%% 
%% This software is released under the MIT License.
%% https://opensource.org/licenses/MIT
-module(test_eunit_frappe).
-include_lib("eunit/include/eunit.hrl").
-export([test_eunit_test_all/0]).
test_eunit_test_all()->eunit:test(test_suite(), [verbose]).
test_suite()->
    [ {"Basic Test", spawn,
  [
    test_start_and_stop_server(),
    test_set(),
    test_read(),
    test_insert(),
    test_update(),
    test_upsert(),
    test_upsert_block_upsert(),
    test_all_items()
  ]}
  ].

test_upsert_block_upsert() ->
    { timeout,1000,
        fun() ->
          {ok, FS} = frappe:fresh(10),
          ok = frappe:set(FS, key1, [a,b], 2),
          spawn(fun() -> frappe:upsert(FS, key2,
                                        fun(new) ->
                                            New = [a, b] ++ [c,d],
                                            timer:sleep(5),
                                            {new_value, New, length(New)}
                                        end) end),
          timer:sleep(10),
          spawn(fun() -> frappe:upsert(FS, key2,
                                        fun({existing, Val}) ->
                                            New = Val ++ [e,f],
                                            timer:sleep(10),
                                            {new_value, New, length(New)}
                                        end) end),
          timer:sleep(40),
          ?assertMatch({ok,[a,b,c,d,e,f]}, frappe:read(FS, key2))
        end}.
  test_all_items()->[
    {
      "Basic Test All item",
      fun()->
        {ok,FS}=frappe:fresh(4),
      ?assertEqual(ok,frappe:insert(FS, key1, [d,d], 3)),
      ?assertEqual(ok, frappe:upsert(FS, key1,
      fun({existing, Val}) ->
        New = Val ++ [d],
        {new_value, New, length(New)}
      end
    )),
    ?assertEqual(ok,frappe:insert(FS, key2, [d,a], 1)),
    ?assertEqual({ok,[d,d,d]},frappe:read(FS, key1)),
    ?assertEqual(ok, frappe:update(FS, key1, 1, 3)),
    ?assertEqual([{key1,1,3},{key2,[d,a],1}], frappe:all_items(FS))
      end
    }
  ].
  test_upsert()->
    [
      {"Basic Upsert test",
    fun()->
      {ok,FS}=frappe:fresh(4),
      ?assertEqual(ok,frappe:insert(FS, key1, [d,d], 3)),
      ?assertEqual(ok, frappe:upsert(FS, key1,
      fun({existing, Val}) ->
        New = Val ++ [d],
        {new_value, New, length(New)}
      end
    )),
    timer:sleep(10),
    ?assertEqual(ok,frappe:insert(FS, key2, [d,a], 1)),
    ?assertEqual({ok,[d,d,d]},frappe:read(FS, key1)),
    ?assertEqual(ok, frappe:update(FS, key1, 1, 3)),
    ?assertEqual({ok,1},frappe:read(FS, key1))
    end
  },
  {
    "Basic Upsert test 2",
    fun()->
      {ok,FS}=frappe:fresh(4),
      ?assertEqual(ok,frappe:insert(FS, key1, [d,d], 3)),
      ?assertEqual(ok, frappe:upsert(FS, key1,
      fun({existing, Val}) ->
        New = Val ++ [d],
        throw({new_value, New, length(New)})
      end
    )),
    ?assertEqual(ok,frappe:insert(FS, key2, [d,a], 1)),
    ?assertEqual({ok,[d,d,d]},frappe:read(FS, key1)),
    ?assertEqual(ok, frappe:update(FS, key1, 1, 3)),
    ?assertEqual({ok,1},frappe:read(FS, key1))
    end
  },
  {
    "Basic Upsert test 3",
    fun()->
      {ok,FS}=frappe:fresh(4),
      ?assertEqual(ok,frappe:insert(FS, key1, [d,d], 3)),
      ?assertEqual(ok, frappe:upsert(FS, key2,
      fun(new) ->
        New = [d,d],
        throw({new_value, New, length(New)})
      end
    )),
    timer:sleep(10),
    ?assertMatch({error,_},frappe:insert(FS, key2, [d,a], 1)),
    ?assertEqual({ok,[d,d]},frappe:read(FS, key2)),
    ?assertMatch({error,_}, frappe:update(FS, key1, 1, 3)),
    ?assertEqual(nothing,frappe:read(FS, key1))
    end
  },
  {
    "Basic Upsert test 4",
    fun()->
      {ok,FS}=frappe:fresh(4),
      ?assertEqual(ok,frappe:insert(FS, key1, [d,d], 3)),
      ?assertEqual(ok, frappe:upsert(FS, key2,
      fun(new) ->
        % New = [d,d],
        throw(1)
      end
    )),
    ?assertEqual(ok,frappe:insert(FS, key2, [d,a], 1)),
    ?assertEqual({ok,[d,a]},frappe:read(FS, key2)),
    ?assertEqual(ok, frappe:update(FS, key1, 1, 3)),
    ?assertEqual({ok,1},frappe:read(FS, key1))
    end
  },
  {
    "Basic Upsert test 5",
    fun()->
      {ok,FS}=frappe:fresh(4),
      ?assertEqual(ok,frappe:insert(FS, key1, [d,d], 3)),
      ?assertEqual(ok, frappe:upsert(FS, key2,
      fun(new) ->
        % New = [d,d],
        [das]
      end
    )),
    ?assertEqual(ok,frappe:insert(FS, key2, [d,a], 1)),
    ?assertEqual({ok,[d,a]},frappe:read(FS, key2)),
    ?assertEqual(ok, frappe:update(FS, key1, 1, 3)),
    ?assertEqual({ok,1},frappe:read(FS, key1))
    end
  }
  ,
  {
    "Upsert Test --Block 1",
    fun()->
      {ok, FS} = frappe:fresh(5),
        ok = frappe:set(FS, key1, [a,b], 2),
        ok = frappe:set(FS, key2, [a,b,c], 2),
        ok = frappe:set(FS, big, [a,b,c,d], 3),
        spawn(fun() -> frappe:upsert(FS, key2,
                                      fun({existing, Val}) ->
                                          New = Val ++ [a],
                                          timer:sleep(100),
                                          {new_value, New, length(New)}
                                      end) end),
        timer:sleep(10),
        frappe:set(FS, key2, [e,f,g], 3),
        timer:sleep(30),
        ?assertMatch({ok, [e, f, g]}, frappe:read(FS, key2))
    end
  },
  {
    "Upsert Test --Block 2",
    fun()->
      {ok, FS} = frappe:fresh(5),
        ok = frappe:set(FS, key1, [a,b], 2),
        ok = frappe:set(FS, big, [a,b,c,d], 3),
  
        spawn(fun() -> frappe:upsert(FS, key2,
                                      (fun(new) ->
                                          timer:sleep(50),
                                          {new_value, 1, 1}
                                      end)) end),
        timer:sleep(40),
        ?assertMatch({error,_}, frappe:insert(FS, key2, 32, 1))
    end
  },
  {
    "Upsert Test --Block 3",
    fun()->
      {ok, FS} = frappe:fresh(5),
        ok = frappe:set(FS, key1, [a,b], 2),
        ok = frappe:set(FS, big, [a,b,c,d], 3),
        spawn(fun() -> frappe:upsert(FS, key2,
                                      (fun(new) ->
                                          timer:sleep(50),
                                          {new_value, 1, 1}
                                      end)) end),
        timer:sleep(30),
        ?assertMatch(ok, frappe:update(FS, key2, 32, 1)),
        ?assertEqual({ok,32}, frappe:read(FS, key2))
    end
  }
  ].
  
  test_update()->
    [
      {
        "Basic Update test",
        fun()->
          {ok,FS}=frappe:fresh(4),
          ?assertEqual(ok,frappe:insert(FS, key1, [d,d], 3)),
          ?assertEqual(ok,frappe:insert(FS, key2, [d,a], 1)),
          ?assertEqual({ok,[d,d]},frappe:read(FS, key1)),
          ?assertEqual(ok, frappe:update(FS, key1, 1, 3)),
          ?assertEqual({ok,1},frappe:read(FS, key1)),
          ?assertMatch({error,_}, frappe:update(FS, key3, 1, 3))
        end
      },
      {
        "Basic Update test --LRU",
        fun()->
          {ok,FS}=frappe:fresh(4),
          ?assertEqual(ok,frappe:insert(FS, key1, [d,d], 3)),
          ?assertEqual(ok,frappe:insert(FS, key2, [d,a], 1)),
          ?assertEqual({ok,[d,d]},frappe:read(FS, key1)),
          ?assertEqual(ok, frappe:update(FS, key1, 1, 3)),
          ?assertEqual({ok,1},frappe:read(FS, key1)),
          ?assertEqual(ok, frappe:insert(FS,key3,[ss],1)),
          ?assertEqual({ok,1},frappe:read(FS, key1)),
          ?assertEqual(nothing,frappe:read(FS, key2))
        end
      },
      {
        "Basic Update test2",
        fun()->
          {ok,FS}=frappe:fresh(50),
          frappe:set(FS,0,97,3),
frappe:update(FS,0,97,3),
frappe:set(FS,1,97,8),
frappe:set(FS,2,97,9),
spawn(fun()->
frappe:update(FS,0,97,9)end),
frappe:set(FS,11,97,20),
frappe:set(FS,3,97,3),
frappe:update(FS,1,97,13),
frappe:update(FS,0,97,11),
frappe:update(FS,1,97,7),
frappe:update(FS,4,97,20),
frappe:set(FS,0,97,3),
frappe:update(FS,0,97,3)
          
          % ?assertEqual(ok,frappe:update(FS, key2, [d,e], 6)),
          
        end
      }
    ].
  test_insert()->
    [
      {"Basic Insert Test",
      fun()->
        {ok,FS}=frappe:fresh(4),
        ?assertEqual(ok,frappe:insert(FS, key1, [d,d], 3)),
        ?assertEqual(ok,frappe:insert(FS, key2, [d,a], 1)),
        ?assertEqual({ok,[d,d]},frappe:read(FS, key1)),
        ?assertEqual({ok,[d,a]},frappe:read(FS, key2)),
        ?assertMatch({error,_},frappe:insert(FS, key2, [d,a], 5))
      end
      },
      {
        "Basic Insert Test LRU",
        fun()->
          {ok,FS}=frappe:fresh(4),
          ?assertEqual(ok,frappe:insert(FS, key1, [d,d], 3)),
          ?assertEqual(ok,frappe:insert(FS, key2, [d,a], 1)),
          ?assertMatch({error,_},frappe:insert(FS, key1, as, 1)),
          ?assertEqual(ok, frappe:insert(FS, key3, [d,b], 1)),
          ?assertEqual({ok,[d,b]},frappe:read(FS, key3)),
          ?assertEqual(nothing,frappe:read(FS, key1))
        end
      }
    ].
  test_read()->[
    { "Basic Read test",
    fun()->
      {ok,FS}=frappe:fresh(4),
      ?assertEqual(ok,frappe:set(FS, key1, [a,b], 2)),
      ?assertEqual({ok,[a,b]}, frappe:read(FS, key1))
    end
    },
    {
      "Basic Read test---LRU",
    fun()->
      {ok,FS}=frappe:fresh(4),
      ?assertEqual(ok,frappe:set(FS, key1, [a,b], 2)),
      ?assertEqual({ok,[a,b]}, frappe:read(FS, key1)),
      ?assertEqual(ok,frappe:set(FS, key2, [a,c], 3)),
      ?assertEqual({ok,[a,c]}, frappe:read(FS, key2)),
      ?assertEqual(nothing, frappe:read(FS, key1))
    end
    },
    {
      "Basic Read test---LRU2",
      fun()->
        {ok,FS}=frappe:fresh(4),
        ?assertEqual(ok,frappe:set(FS, key1, [a,b], 2)),
        ?assertEqual({ok,[a,b]}, frappe:read(FS, key1)),
        ?assertEqual(ok,frappe:set(FS, key2, [a,c], 3)),
        ?assertEqual({ok,[a,c]}, frappe:read(FS, key2)),
        ?assertEqual(nothing, frappe:read(FS, key1)),
        ?assertEqual(ok,frappe:set(FS, key3, [a,c], 1)),
        ?assertEqual({ok,[a,c]}, frappe:read(FS, key3)),
        ?assertEqual({ok,[a,c]}, frappe:read(FS, key2)),
        ?assertEqual(nothing, frappe:read(FS, key1)),
        ?assertEqual(ok,frappe:set(FS, key3, [a,c], 2)),
        ?assertEqual({ok,[a,c]}, frappe:read(FS, key3)),
        ?assertEqual(nothing, frappe:read(FS, key2))
      end
    }
  
  ].
  test_set()->[
    {"Basic set test",
    fun()->
      {ok,FS}=frappe:fresh(4),
      ?assertEqual(ok,frappe:set(FS, key1, [a,b], 2)),
      ?assertEqual(ok,frappe:set(FS, key2, [a,b], 4)),
      ?assertMatch({error,_},frappe:set(FS, key3, [a,b], 5))
    end}
  ].
  test_start_and_stop_server()->[
    {"Basic start server",
    fun()->
      ?assertMatch({ok,_}, frappe:fresh(4))
    end},
    {"Basic start and stop server",
    fun()->
      {ok,FS}=frappe:fresh(4),
      ?assertEqual(ok, frappe:stop(FS))
    end}
  ].
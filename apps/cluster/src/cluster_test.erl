-module(cluster_test).

-include_lib("eunit/include/eunit.hrl").

-define(RETRY_ATTEMPTS, 120).
-define(RETRY_SLEEP, 1000).
-define(TEST_TIMEOUT, 300).
-define(DEFAULT_ENDPOINT, "https://front-pedro-gutierrez.cloud.okteto.net").

endpoint() ->
    case os:getenv("TEST_ENDPOINT") of
        false ->
            ?DEFAULT_ENDPOINT;
        Endpoint ->
            Endpoint
    end.

cluster_test_() ->
    {timeout,
     ?TEST_TIMEOUT,
     fun() ->
        test_halt_host(),
        test_disconnect_host(),
        test_netsplit_manual_recovery(),
        test_netsplit_automatic_recovery()
     end}.

test_halt_host() ->
    print("~n== TEST test_halt_host()"),
    setup(),
    assert_cluster_state(<<"green">>),
    Leader = cluster_leader(),
    halt_host(Leader),
    assert_cluster_state(<<"red">>),
    refute_cluster_leader(Leader),
    join_host(Leader),
    assert_cluster_state(<<"green">>).

test_disconnect_host() ->
    print("~n== TEST test_disconnect_host()"),
    setup(),
    assert_cluster_state(<<"green">>),
    Hosts = cluster_hosts(),
    disconnect_hosts_and_wait_for_cluster_state(Hosts, <<"red">>),
    join_hosts_and_wait_for_cluster_state(Hosts, <<"green">>).

test_netsplit_manual_recovery() ->
    print("~n== TEST test_netsplit_manual_recovery()"),
    setup(),
    assert_cluster_state(<<"green">>),
    set_cluster_recovery("manual"),
    Hosts = cluster_hosts(),
    delete_all_keys(),
    write_keys("a", 100),
    disconnect_hosts_and_wait_for_cluster_state(Hosts, <<"red">>),
    write_keys("b", 100),
    {Host, Size} = busiest_host(),
    join_hosts_and_wait_for_cluster_state(Hosts, <<"green">>),
    assert_cluster_leader(Host),
    assert_store_size(Size).

test_netsplit_automatic_recovery() ->
    print("~n== TEST test_netsplit_automatic_recovery()"),
    setup(),
    assert_cluster_state(<<"green">>),
    set_cluster_recovery("auto"),
    Hosts = cluster_hosts(),
    delete_all_keys(),
    write_keys("a", 100),
    disconnect_hosts_and_wait_for_cluster_state(Hosts, <<"red">>),
    write_keys("b", 100),
    {Host, Size} = busiest_host(),
    join_hosts_and_wait_for_cluster_state(Hosts, <<"green">>),
    assert_cluster_leader(Host),
    assert_store_size(Size).

assert_cluster_state(State) ->
    print("asserting cluster state ~p", [State]),
    retry(fun() -> do_assert_cluster_state(State) end,
          <<"cluster not in state: ", State/binary>>),
    ok.

do_assert_cluster_state(State) ->
    Url = endpoint(),
    {ok, #{status := 200, body := #{<<"state">> := State}}} = http(Url).

assert_cluster_leader(Host) ->
    print("asserting cluster leader ~p", [Host]),
    retry(fun() ->
             Url = endpoint(),
             {ok, #{status := 200, body := #{<<"leader">> := Host}}} = http(Url)
          end,
          <<"expected cluster to be", Host/binary>>).

refute_cluster_leader(Host) ->
    print("refuting cluster leader ~p", [Host]),
    retry(fun() ->
             Url = endpoint(),
             {ok, #{status := 200, body := #{<<"leader">> := Leader}}} = http(Url),
             ?assert(Host =/= Leader)
          end,
          <<"expected cluster leader not to be ", Host/binary>>).

cluster_leader() ->
    print("retrieving cluster leader"),
    {ok, Leader} =
        retry(fun() ->
                 Url = endpoint(),
                 {ok, #{status := 200, body := #{<<"leader">> := Leader}}} = http(Url),
                 {ok, Leader}
              end,
              <<"cluster does not have a leader">>),
    Leader.

assert_store_size(Size) ->
    print("asserting store size ~p", [Size]),
    retry(fun() ->
             Size = store_size(),
             ok
          end,
          <<"Expected store size should be ", (erlang:integer_to_binary(Size))/binary>>).

store_size() ->
    Url = endpoint(),
    {ok, #{status := 200, body := #{<<"store">> := #{<<"size">> := Size}}}} = http(Url),
    Size.

halt_host(Host) ->
    print("halting host ~p", [Host]),
    retry(fun() ->
             Path = erlang:binary_to_list(<<"/hosts/", Host/binary, "?mode=halt">>),
             Url = url(Path),
             {ok, #{status := 200}} = http(delete, Url)
          end,
          <<"could not halt host ", Host/binary>>).

disconnect_hosts_and_wait_for_cluster_state(Hosts, State) ->
    print("disconnecting hosts ~p and waiting for cluster state ~p", [Hosts, State]),
    retry(fun() ->
             disconnect_hosts(Hosts),
             do_assert_cluster_state(State)
          end,
          <<"disconnecting hosts did not result in a ", State/binary, " cluster state">>).

disconnect_hosts(Hosts) ->
    lists:foreach(fun(Host) ->
                     Path = erlang:binary_to_list(<<"/hosts/", Host/binary, "?mode=disconnect">>),
                     Url = url(Path),
                     {ok, #{status := 200}} = http(delete, Url)
                  end,
                  Hosts).

join_host(Host) ->
    retry(fun() -> do_join_host(Host) end, <<"could not join host ", Host/binary>>).

do_join_hosts(Hosts) ->
    lists:foreach(fun(Host) ->
                     Path = erlang:binary_to_list(<<"/hosts/", Host/binary>>),
                     Url = url(Path),
                     Res = http(post, Url),
                     {ok, #{status := 200}} = Res
                  end,
                  Hosts).

do_join_host(Host) ->
    print("joining host ~p", [Host]),
    retry(fun() ->
             Path = erlang:binary_to_list(<<"/hosts/", Host/binary>>),
             Url = url(Path),
             Res = http(post, Url),
             {ok, #{status := 200}} = Res
          end,
          <<"could not join host ", Host/binary>>).

join_hosts_and_wait_for_cluster_state(Hosts, State) ->
    print("joining hosts ~p and waiting for cluster state ~p", [Hosts, State]),
    retry(fun() ->
             do_join_hosts(Hosts),
             do_assert_cluster_state(State)
          end,
          <<"joining hosts did not result in a ", State/binary, " cluster state">>).

cluster_hosts() ->
    print("getting cluster hosts"),
    {ok, Hosts} =
        retry(fun() ->
                 Url = endpoint(),
                 {ok, #{status := 200, body := #{<<"nodes">> := Hosts}}} = http(Url),
                 {ok, Hosts}
              end,
              <<"could not get cluster hosts">>),
    Hosts.

delete_all_keys() ->
    print("deleting all keys"),
    retry(fun() ->
             Url = url("/keys"),
             {ok, #{status := 200}} = http(delete, Url)
          end,
          <<"could not delete keys">>).

write_keys(Prefix, N) ->
    BatchSize = 10,
    NumberOfBatches = floor(N / BatchSize),
    print("writing ~p keys with prefix ~p in ~p batches of ~p",
          [N, Prefix, NumberOfBatches, BatchSize]),
    retry(fun() -> write_key_batches(Prefix, BatchSize, NumberOfBatches) end,
          <<"could not write keys">>).

write_key_batches(_, _, 0) ->
    ok;
write_key_batches(Prefix, BatchSize, BatchNumber) ->
    progress(),
    write_key_batch(Prefix, BatchSize, BatchNumber),
    write_key_batches(Prefix, BatchSize, BatchNumber - 1).

write_key_batch(Prefix, BatchSize, BatchNumber) ->
    To = BatchNumber * BatchSize,
    From = (BatchNumber - 1) * BatchSize + 1,
    write_keys(Prefix, From, To).

write_keys(Prefix, From, To) ->
    Keys = lists:seq(From, To),
    lists:foreach(fun(I) ->
                     K = Prefix ++ erlang:integer_to_list(I),
                     {ok, #{status := 200}} = do_write_key(K, "value")
                  end,
                  Keys).

do_write_key(K, V) ->
    Url = url("/keys/key" ++ K),
    Resp = http(put, Url, [], V),
    Resp.

busiest_host() ->
    Hosts = cluster_hosts(),
    Score = lists:foldl(fun(H, Map) -> maps:put(H, 0, Map) end, #{}, Hosts),
    busiest_host(Score, 10).

busiest_host(Score) ->
    [Default | _] = cluster_hosts(),
    maps:fold(fun(Host, Size, {_, Size0} = Current) ->
                 case Size > Size0 of
                     true -> {Host, Size};
                     _ -> Current
                 end
              end,
              {Default, 0},
              Score).

busiest_host(Score, 0) ->
    busiest_host(Score);
busiest_host(Score, IterationsLeft) ->
    {Host, Size} = cluster_store_size(),
    Score2 = maps:put(Host, Size, Score),

    busiest_host(Score2, IterationsLeft - 1).

cluster_store_size() ->
    Url = endpoint(),
    {ok,
     #{status := 200,
       headers := #{<<"host">> := Host},
       body := #{<<"store">> := #{<<"size">> := Size}}}} =
        http(Url),
    {Host, Size}.

set_cluster_recovery(Recovery) ->
    print("setting cluster recovery to ~p", [Recovery]),
    retry(fun() ->
             Url = url("/?recovery=" ++ Recovery),
             {ok, #{status := 200, body := #{<<"recovery">> := Recovery}}} = http(put, Url)
          end,
          <<"could not set cluster recovery">>).

setup() ->
    inets:start(),
    ssl:start().

url(Path) ->
    url(endpoint(), Path).

url(Base, Path) ->
    Base ++ Path.

retry(Fun, Msg) ->
    retry(?RETRY_ATTEMPTS, ?RETRY_SLEEP, Fun, Msg).

retry(0, _, _, Msg) ->
    throw(Msg);
retry(Attempts, Sleep, Fun, Msg) ->
    progress(),
    case safe(Fun) of
        ok ->
            ok;
        {ok, _} = Result ->
            Result;
        {error, _} = _Err ->
            timer:sleep(Sleep),
            retry(Attempts - 1, Sleep, Fun, Msg)
    end.

safe(Fun) ->
    try
        Fun()
    catch
        Type:Error ->
            {error, {Type, Error}}
    end.

http(Url) ->
    http(get, Url).

http(post, Url) ->
    http(post, Url, [], "");
http(Method, Url) ->
    http(Method, Url, []).

http(Method, Url, Headers) ->
    Request = {Url, Headers},
    request(Method, Request).

http(Method, Url, Headers, Body) ->
    Req = {Url, Headers, "", Body},
    Resp = request(Method, Req),
    maybe_print(#{request => Req, response => Resp}),
    Resp.

request(Method, Request) ->
    case httpc:request(Method, Request, [], []) of
        {ok, {{_, Status, _}, Headers, Body}} ->
            case decode_json(Body) of
                {ok, DecodedBody} ->
                    DecodedHeaders =
                        lists:foldl(fun({K, V}, Map) ->
                                       maps:put(
                                           erlang:list_to_binary(K), erlang:list_to_binary(V), Map)
                                    end,
                                    #{},
                                    Headers),
                    {ok,
                     #{status => Status,
                       headers => DecodedHeaders,
                       body => DecodedBody}};
                {error, _} ->
                    {error, {invalid_json, Body}}
            end;
        {error, _} = Err ->
            Err
    end.

decode_json(<<>>) ->
    {ok, <<>>};
decode_json([]) ->
    {ok, <<>>};
decode_json(Raw) ->
    try
        {ok, jiffy:decode(Raw, [return_maps])}
    catch
        _:_ ->
            {error, raw}
    end.

print(Msg) ->
    print(Msg, []).

print(Msg, Args) ->
    io:format(user, "~n" ++ Msg ++ " ", Args).

maybe_print(Term) ->
    case os:getenv("TEST_DEBUG") of
        "true" ->
            print("~p", [Term]);
        _ ->
            ok
    end.

progress() ->
    io:format(user, ".", []).

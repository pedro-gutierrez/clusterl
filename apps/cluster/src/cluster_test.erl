-module(cluster_test).

-include_lib("eunit/include/eunit.hrl").

-define(RETRY_ATTEMPTS, 120).
-define(RETRY_SLEEP, 1000).
-define(TEST_TIMEOUT, 300).

cluster_test_() ->
    {timeout,
     ?TEST_TIMEOUT,
     fun() ->
        test_halt_host(),
        test_disconnect_host(),
        test_netsplit()
     end}.

test_halt_host() ->
    print("== TEST test_halt_host()"),
    setup(),
    assert_cluster_state(<<"green">>),
    Leader = cluster_leader(),
    halt_host(Leader),
    assert_cluster_state(<<"red">>),
    refute_cluster_leader(Leader),
    join_host(Leader),
    assert_cluster_state(<<"green">>).

test_disconnect_host() ->
    print("== TEST test_disconnect_host()"),
    setup(),
    assert_cluster_state(<<"green">>),
    Hosts = cluster_hosts(),
    disconnect_hosts_and_wait_for_cluster_state(Hosts, <<"red">>),
    join_hosts_and_wait_for_cluster_state(Hosts, <<"green">>).

test_netsplit() ->
    print("== TEST test_netsplit()"),
    setup(),
    assert_cluster_state(<<"green">>),
    Hosts = cluster_hosts(),
    delete_all_keys(),
    write_keys(100),
    disconnect_hosts_and_wait_for_cluster_state(Hosts, <<"red">>),
    write_keys(100).

assert_cluster_state(State) ->
    retry(fun() -> do_assert_cluster_state(State) end,
          <<"cluster not in state: ", State/binary>>),
    ok.

do_assert_cluster_state(State) ->
    print("asserting cluster state ~p", [State]),
    Url = endpoint(),
    {ok, #{status := 200, body := #{<<"state">> := State}}} = http(Url).

refute_cluster_leader(Host) ->
    print("refuting cluster leader ~p", [Host]),
    retry(fun() ->
             Url = endpoint(),
             {ok, #{status := 200, body := #{<<"leader">> := Leader}}} = http(Url),
             ?assert(Host =/= Leader)
          end,
          <<"expected cluster leader to be ", Host/binary>>).

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

halt_host(Host) ->
    print("halting host ~p", [Host]),
    retry(fun() ->
             Path = erlang:binary_to_list(<<"/hosts/", Host/binary, "?mode=halt">>),
             Url = url(Path),
             {ok, #{status := 200}} = http(delete, Url)
          end,
          <<"could not halt host ", Host/binary>>).

disconnect_hosts_and_wait_for_cluster_state(Hosts, State) ->
    retry(fun() ->
             disconnect_hosts(Hosts),
             do_assert_cluster_state(State)
          end,
          <<"disconnecting hosts did not result in a ", State/binary, " cluster state">>).

disconnect_hosts(Hosts) ->
    print("disconnect hosts ~p", [Hosts]),
    lists:foreach(fun(Host) ->
                     Path = erlang:binary_to_list(<<"/hosts/", Host/binary, "?mode=disconnect">>),
                     Url = url(Path),
                     {ok, #{status := 200}} = http(delete, Url)
                  end,
                  Hosts).

join_host(Host) ->
    retry(fun() -> do_join_host(Host) end, <<"could not join host ", Host/binary>>).

do_join_hosts(Hosts) ->
    print("joining hosts ~p", [Hosts]),
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

write_keys(N) ->
    print("writing ~p keys", [N]),
    retry(fun() ->
             lists:foreach(fun(K) ->
                              Url = url("/keys/key" ++ K),
                              {ok, #{status := 200}} = http(put, Url, <<"value">>)
                           end,
                           lists:seq(1, N))
          end,
          <<"could not write keys">>).

setup() ->
    inets:start(),
    ssl:start().

url(Path) ->
    url(endpoint(), Path).

url(Base, Path) ->
    Base ++ Path.

                                                % host(N) when is_integer(N) ->
                                                %     Id = erlang:integer_to_binary(N),
                                                %     Service = cluster:env("CLUSTER_SERVICE"),
                                                %     <<Service/binary, "-", Id>>.

endpoint() ->
    cluster:env("CLUSTER_ENDPOINT").

retry(Fun, Msg) ->
    retry(?RETRY_ATTEMPTS, ?RETRY_SLEEP, Fun, Msg).

retry(0, _, _, Msg) ->
    throw(Msg);
retry(Attempts, Sleep, Fun, Msg) ->
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
    Request = {Url, Headers, "", Body},
    request(Method, Request).

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
    io:format(user, Msg ++ "~n", Args).

% join_binaries([A, B]) ->
%     <<A/binary, ",", B/binary>>.

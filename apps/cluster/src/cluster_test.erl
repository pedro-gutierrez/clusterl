-module(cluster_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(RETRY_ATTEMPTS, 10).
-define(RETRY_SLEEP, 1000).
-define(TEST_TIMEOUT, 60).

cluster_test_() ->
    {timeout, ?TEST_TIMEOUT, [{test, ?MODULE, change_leader_scenario}]}.

change_leader_scenario() ->
    setup(),
    assert_cluster_state(<<"green">>),
    _Leader = cluster_leader(),
    %io:format(user, "~p", [Leader]),
    ok.

assert_cluster_state(Expected) ->
    retry(fun() ->
             Url = endpoint(),
             {ok, #{status := 200, body := #{<<"state">> := Expected}}} = Resp = http(Url),
             Resp
          end,
          <<"cluster not in state: ", Expected/binary>>),
    ok.

cluster_leader() ->
    {ok, Leader} =
        retry(fun() ->
                 Url = endpoint(),
                 {ok, #{status := 200, body := #{<<"leader">> := Leader}}} = http(Url),
                 {ok, Leader}
              end,
              <<"cluster does not have a leader">>),
    Leader.

                                                % cluster_info() ->
                                                %     Url = endpoint(),
                                                %     retry(fun() ->
                                                %         http(Url)
                                                %     end, "cluster not available at " ++ Url).

setup() ->
    inets:start(),
    ssl:start().

%%url(Base, Path) ->
%%    Base ++ Path.

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
        {ok, _} = Result ->
            Result;
        {error, _} = Err ->
            io:format(user, "~p", [Err]),
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

decode_json(Raw) ->
    try
        {ok, jiffy:decode(Raw, [return_maps])}
    catch
        _:_ ->
            {error, raw}
    end.

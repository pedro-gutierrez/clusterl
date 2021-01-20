-module(cluster_http_info).

-export([init/2]).

init(Req, _) ->
    Method = cowboy_req:method(Req),
    Recovery = recovery(Req),
    do(Method, Recovery, Req).

do(<<"GET">>, _, Req) ->
    Leader =
        cluster_http:host(
            cluster:leader()),
    Hosts = cluster_http:hosts([node() | nodes()]),

    cluster_http:ok(#{recovery => cluster:recovery(),
                      state => cluster:state(),
                      size => cluster:size(),
                      leader => Leader,
                      nodes => Hosts,
                      store => cluster_store:info()},
                    Req);
do(<<"PUT">>, Recovery, Req) ->
    ok = cluster:set_recovery(Recovery),
    do(<<"GET">>, Recovery, Req).

recovery(Req) ->
    case cowboy_req:match_qs([recovery], Req) of
        #{recovery := <<"manual">>} ->
            manual;
        _ ->
            auto
    end.

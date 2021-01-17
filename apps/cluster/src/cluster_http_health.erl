-module(cluster_http_health).

-export([init/2]).

init(Req, _) ->
    Leader = cluster_http:host(cluster:leader()),
    Hosts = cluster_http:hosts([node() | nodes()]),

    cluster_http:ok(#{state => cluster:state(),
                      size => cluster:size(),
                      leader => Leader,
                      nodes => Hosts,
                      store => cluster_store:info()},
                    Req).


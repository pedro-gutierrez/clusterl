-module(cluster_sup).

-behaviour(supervisor).

-export([init/1, start_link/0]).

start_link() ->
    ChildsSpec =
        [worker(cluster_monitor, [cluster:neighbours()]),
         supervisor(cluster_leader_sup),
         worker(cluster_leader_monitor),
         worker(cluster_http, [cluster:http_port()]),
         worker(cluster_store)],
    supervisor:start_link({local, ?MODULE}, ?MODULE, {{one_for_one, 10, 60}, ChildsSpec}).

init(ChildSpecs) ->
    {ok, ChildSpecs}.

worker(Mod) ->
    worker(Mod, []).

worker(Mod, Args) ->
    {Mod, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}.

supervisor(Mod) ->
    supervisor(Mod, []).

supervisor(Mod, Args) ->
    {Mod, {Mod, start_link, Args}, permanent, 5000, supervisor, [Mod]}.

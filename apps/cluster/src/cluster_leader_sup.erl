-module(cluster_leader_sup).

-behaviour(supervisor).

-export([init/1, start_link/0, start_leader/0]).

start_link() ->
    ChildsSpec = [worker(cluster_leader)],
    supervisor:start_link({local, ?MODULE},
                          ?MODULE,
                          {{simple_one_for_one, 10, 60}, ChildsSpec}).

init(ChildSpecs) ->
    {ok, ChildSpecs}.

worker(Mod) ->
    worker(Mod, []).

worker(Mod, Args) ->
    {Mod, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}.

start_leader() ->
    supervisor:start_child(?MODULE, []).

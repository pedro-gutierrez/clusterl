-module(cluster_monitor).

-behaviour(gen_server).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link(Neighbours) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Neighbours, []).

init([]) ->
    ok = pg2:create(cluster_events),
    lager:notice("CLUSTER is disabled"),
    {ok, #{state => disabled, neighbours => []}};
init(Neighbours) ->
    ok = pg2:create(cluster_events),
    global_group:monitor_nodes(true),
    cluster:join(Neighbours),
    ClusterState = cluster:state(Neighbours),
    lager:notice("CLUSTER is ~p (neighbours: ~p)~n", [ClusterState, Neighbours]),
    {ok, #{state => ClusterState, neighbours => Neighbours}}.

handle_info({nodeup, N}, #{neighbours := Neighbours} = State) ->
    ClusterState = cluster:state(Neighbours),
    notify_cluster_state(),
    lager:notice("CLUSTER is ~p (~p is UP)~n", [ClusterState, N]),
    {noreply, State#{state => ClusterState}};
handle_info({nodedown, N}, #{neighbours := Neighbours} = State) ->
    ClusterState = cluster:state(Neighbours),
    notify_cluster_state(),
    lager:notice("CLUSTER is ~p (~p is DOWN)~n", [ClusterState, N]),
    {noreply, State#{state => ClusterState}}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, ok, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(Reason, State) ->
    lager:warning("CLUSTER terminating with reason: ~p, and state: ~p~n", [Reason, State]),
    ok.

notify_cluster_state() ->
    [Pid ! {cluster, state} || Pid <- pg2:get_members(cluster_events)].


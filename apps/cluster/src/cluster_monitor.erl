-module(cluster_monitor).

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([set_recovery/1, recovery/0]).

start_link(Neighbours) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Neighbours, []).

set_recovery(Recovery) ->
    gen_server:call(?MODULE, {set_recovery, Recovery}).

recovery() ->
    gen_server:call(?MODULE, recovery).

init([]) ->
    ok = pg2:create(cluster_events),
    lager:notice("CLUSTER is disabled"),
    {ok,
     #{recovery => manual,
       state => disabled,
       neighbours => []}};
init(Neighbours) ->
    ok = pg2:create(cluster_events),
    global_group:monitor_nodes(true),
    ClusterState = cluster:state(Neighbours),
    State =
        #{recovery => auto,
          state => ClusterState,
          neighbours => Neighbours},
    attempt_recovery(State),
    lager:notice("CLUSTER is ~p (neighbours: ~p)~n", [ClusterState, Neighbours]),
    {ok, State}.

handle_info({nodeup, N}, #{neighbours := Neighbours} = State) ->
    ClusterState = cluster:state(Neighbours),
    notify_cluster_state(),
    lager:notice("CLUSTER is ~p (~p is UP)~n", [ClusterState, N]),
    {noreply, State#{state => ClusterState}};
handle_info({nodedown, N}, #{neighbours := Neighbours} = State) ->
    ClusterState = cluster:state(Neighbours),
    notify_cluster_state(),
    lager:notice("CLUSTER is ~p (~p is DOWN)~n", [ClusterState, N]),
    attempt_recovery(State),
    {noreply, State#{state => ClusterState}};
handle_info(attempt_recovery, #{recovery := auto, neighbours := Neighbours} = State) ->
    case cluster:state(Neighbours) of
        red ->
            lager:notice("CLUSTER still red and recovery is auto"),
            attempt_recovery(State);
        green ->
            lager:notice("CLUSTER is green. Stopping recovery for now"),
            ok
    end,

    {noreply, State};
handle_info(attempt_recovery, #{neighbours := Neighbours} = State) ->
    case cluster:state(Neighbours) of
        red ->
            lager:notice("CLUSTER still red, but recovery is manual. Recovery won't be "
                         "attempted"),
            ok;
        _ ->
            ok
    end,

    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call({set_recovery, Recovery}, _, State) ->
    State2 = State#{recovery => Recovery},
    lager:notice("CLUSTER recovery is set to ~p", [Recovery]),
    attempt_recovery(State2),
    {reply, ok, State2};
handle_call(recovery, _, #{recovery := Recovery} = State) ->
    {reply, Recovery, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(Reason, State) ->
    lager:warning("CLUSTER terminating with reason: ~p, and state: ~p~n", [Reason, State]),
    ok.

notify_cluster_state() ->
    [Pid ! {cluster, state} || Pid <- pg2:get_members(cluster_events)].

attempt_recovery(#{neighbours := Neighbours, recovery := auto}) ->
    % Under certain network conditions, pinging other networks
    % might be slow or timeout. If so, we want to do in a separate
    % context without blocking this cluster monitor
    lager:notice("CLUSTER auto recovery triggered"),
    timer:send_after(5000, self(), attempt_recovery),
    spawn_link(fun() -> cluster:join(Neighbours) end);
attempt_recovery(_) ->
    ok.

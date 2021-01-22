-module(cluster_leader).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([attempt_leader/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ok = pg2:join(cluster_events, self()),
    attempt_leader(),
    lager:notice("CLUSTER has new leader ~p~n", [node()]),
    {ok, []}.

handle_info({cluster, nodes_changed}, State) ->
    attempt_leader(),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, ok, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(Reason, _State) ->
    lager:notice("CLUSTER leader process terminated with reason ~p~n", [Reason]),
    ok.

attempt_leader() ->
    case global:register_name(cluster_leader, self()) of
        yes ->
            lager:notice("CLUSTER has new leader ~p~n", [node()]),
            cluster:notify_observers({cluster, leader_changed});
        no ->
            Pid = global:whereis_name(cluster_leader),
            lager:notice("CLUSTER already has leader ~p~n", [node(Pid)])
    end.

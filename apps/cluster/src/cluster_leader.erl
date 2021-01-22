-module(cluster_leader).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% -export([attempt_leader/0]).

start_link() ->
    gen_server:start_link({global, cluster_leader}, ?MODULE, [], []).

init(_) ->
    ok = pg2:create(leader_events),
    ok = pg2:join(cluster_events, self()),
    notify_cluster_leader(),
    lager:notice("CLUSTER has new leader ~p~n", [node()]),
    {ok, []}.

handle_info(cluster_changed, State) ->
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
            notify_cluster_leader();
        no ->
            Pid = global:whereis_name(cluster_leader),
            lager:notice("CLUSTER already has leader ~p~n", [node(Pid)])
    end.

notify_cluster_leader() ->
    [Pid ! leader_changed || Pid <- pg2:get_members(leader_events)].

-module(cluster_leader).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% -export([attempt_leader/0]).

start_link() ->
    gen_server:start_link({global, cluster_leader}, ?MODULE, [], []).

init(_) ->
    ok = pg2:join(cluster_events, self()),
    notify_cluster_leader(),
    lager:notice("CLUSTER has new leader ~p~n", [node()]),
    {ok, []}.

handle_info(_, State) ->
    {noreply, State}.

% handle_info({cluster, _}, State) ->
%     case cluster:state() of
%         red ->
%             attempt_leader();
%         _ ->
%             ok
%     end,
%     {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, ok, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(Reason, _State) ->
    lager:notice("CLUSTER leader process terminated with reason ~p~n", [Reason]),
    ok.

% attempt_leader() ->
%     case global:register_name(cluster_leader, self(), conflict_resolution_fun()) of
%         yes ->
%             lager:notice("CLUSTER has new leader ~p~n", [node()]),
%             notify_cluster_leader();
%         no ->
%             Pid = global:whereis_name(cluster_leader),
%             lager:notice("CLUSTER already has leader ~p~n", [node(Pid)])
%     end.

notify_cluster_leader() ->
    [Pid ! {cluster, leader} || Pid <- pg2:get_members(cluster_events)].

% conflict_resolution_fun() ->
%     fun(_, Pid1, Pid2) ->
%        Node1 = node(Pid1),
%        Node2 = node(Pid2),
%        #{size := Size1} = rpc:call(Node1, cluster_store, info, []),
%        #{size := Size2} = rpc:call(Node2, cluster_store, info, []),
%        {{WinnerPid, WinnerSize}, {LooserPid, LooserSize}} =
%            case Size1 > Size2 of
%                true -> {{Pid1, Size1}, {Pid2, Size2}};
%                false -> {{Pid2, Size2}, {Pid1, Size1}}
%            end,
%        lager:notice("CLUSTER netsplit winner: ~p (~p keys), looser ~p (~p keys)",
%                     [node(WinnerPid), WinnerSize, node(LooserPid), LooserSize]),
%        WinnerPid
%     end.

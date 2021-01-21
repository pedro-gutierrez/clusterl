-module(cluster_leader_monitor).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([start_leader/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ok = pg2:join(cluster_events, self()),
    {ok, #{leader => undefined, ref => undefined}, 0}.

handle_info(timeout, State) ->
    State2 = start_leader(State),
    {noreply, State2};
handle_info({'DOWN', Ref, process, Pid, Reason}, #{reference := Ref} = State) ->
    lager:notice("CLUSTER leader ~p is down with reason ~p", [node(Pid), Reason]),
    State2 = State#{reference => undefined},
    State3 = start_leader(State2),
    {noreply, State3};
handle_info({cluster, _}, State) ->
    lager:notice("CLUSTER topology has changed, attempting leadership..."),
    State2 = start_leader(State),
    {noreply, State2};
handle_info(_, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, ok, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

start_leader(State) ->
    case start_leader() of
        {ok, Pid} ->
            State#{leader => Pid};
        {ok, Pid, Ref} ->
            lager:notice("CLUSTER already has leader at ~p", [node(Pid)]),
            State#{leader => Pid, reference => Ref}
    end.

start_leader() ->
    case cluster_leader_sup:start_leader() of
        {ok, _} = Ok ->
            Ok;
        {error, {already_started, Pid}} ->
            case node(Pid) =/= node() of
                true ->
                    Ref = process:monitor(process, Pid),
                    {ok, Pid, Ref};
                false ->
                    {ok, Pid}
            end
    end.

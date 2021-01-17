-module(cluster_store).

-behaviour(gen_server).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([write/2, read/1, info/0]).

-record(cluster_items, {key, data}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ok = pg2:join(cluster_events, self()),
    ok = init_store(),
    {ok, _} = mnesia:subscribe(system),
    {ok, []}.

handle_info({cluster, _}, State) ->
    ok = init_store(),
    {noreply, State};
handle_info({mnesia_system_event, {inconsistent_database, Context, Node}}, State) ->
    lager:notice("CLUSTER store netsplit detected by Mnesia: ~p, ~p", [Context, Node]),
    % TODO: increment a counter in prometheus metrics
    {noreply, State};
handle_info(Other, State) ->
    lager:notice("CLUSTER store ignoring ~p", [Other]),
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

init_store() ->
    init_store(cluster:state(), cluster:i_am_leader()).

init_store(green, true) ->
    AllNodes = [node() | nodes()],
    RunningNodes = mnesia:system_info(running_db_nodes),
    MissingMembers = AllNodes -- RunningNodes,
    mnesia:change_config(extra_db_nodes, MissingMembers),
    create_table(AllNodes),
    copy_table(MissingMembers),
    lager:notice("CLUSTER store synced to ~p", [MissingMembers]);
init_store(_, _) ->
    ok.

create_table(Nodes) ->
    mnesia:create_table(cluster_items,
                        [{type, set},
                         {attributes, record_info(fields, cluster_items)},
                         {ram_copies, Nodes}]).

copy_table(Nodes) ->
    [mnesia:add_table_copy(cluster_items, N, ram_copies) || N <- Nodes].

write(Key, Value) ->
    write(#cluster_items{key = Key, data = Value}).

write(#cluster_items{} = Item) ->
    mnesia:activity(transaction,
                    fun () ->
                            mnesia:write(Item)
                    end).

read(Key) ->
    mnesia:activity(transaction,
                    fun () ->
                            case mnesia:read({cluster_items, Key}) of
                              [] ->
                                  {error, not_found};
                              [{_, _, Value}] ->
                                  {ok, Value}
                            end
                    end).

info() ->
    Size = mnesia:table_info(cluster_items, size),
    ActiveReplicas = mnesia:table_info(cluster_items, active_replicas),
    AllReplicas = mnesia:table_info(cluster_items, all_nodes),
    RamCopies = mnesia:table_info(cluster_items, ram_copies),

    #{size => Size,
      ram_copies => cluster_http:hosts(RamCopies),
      replicas =>
          #{all => cluster_http:hosts(AllReplicas), active => cluster_http:hosts(ActiveReplicas)}}.



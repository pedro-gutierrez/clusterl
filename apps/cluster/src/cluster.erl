-module(cluster).

-export([members/0,
         start/0,
         join/1,
         state/0,
         state/1,
         neighbours/0,
         leader/0,
         i_am_leader/0,
         http_port/0,
         size/0,
         service/0,
         namespace/0,
         join/0,
         leave/1]).


state() ->
    state(neighbours()).

state([]) ->
    green;
state(Neighbours) ->
    state(length(nodes()), length(Neighbours)).

state(0, _) ->
    red;
state(N, N) ->
    green;
state(_, _) ->
    yellow.

start() ->
    Neighbours = neighbours(),
    lager:notice("CLUSTER config: neighbours=~p, node=~p~n", [Neighbours, node()]),
    ok.

neighbours() ->
    members() -- [node()].

size() ->
    {ok, Size} = application:get_env(cluster, size),
    Size.

namespace() ->
    {ok, Ns} = application:get_env(cluster, namespace),
    Ns.

service() ->
    {ok, Service} = application:get_env(cluster, service),
    Service.

members() ->
    Ns = namespace(),
    Service = service(),
    Size = size(),
    nodes(Service, Ns, Size).

nodes(_, _, 0) ->
    [];
nodes(Service, Ns, Size) ->
    lists:map(fun (Id) ->
                      node(Service, Ns, erlang:integer_to_binary(Id - 1))
              end,
              lists:seq(1, Size)).

node(Service, Ns, Id) ->
    Host = <<Service/binary,
             "-",
             Id/binary,
             ".",
             Service/binary,
             ".",
             Ns/binary,
             ".svc.cluster.local">>,
    list_to_atom(binary_to_list(<<"netcomp@", Host/binary>>)).

leader() ->
    case global:whereis_name(cluster_leader) of
      undefined ->
          none;
      Pid ->
          node(Pid)
    end.

i_am_leader() ->
    leader() == node().

http_port() ->
    application:get_env(cluster, http_port, 8080).

join() ->
    Neighbours = neighbours(),
    join(Neighbours).

join(Nodes) ->
    [net_adm:ping(N) || N <- Nodes].

leave(normal) ->
    Neighbours = neighbours(),
    leave(Neighbours);
leave(halt) ->
    erlang:halt();

leave(Nodes) ->
    [erlang:disconnect_node(N) || N <- Nodes].
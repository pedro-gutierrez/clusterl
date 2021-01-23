-module(cluster_metrics).

-export([register_metrics/0, inc/1, set/2]).

register_metrics() ->
    prometheus_gauge:new([{name, cluster_expected_size},
                          {help, "Expected size of the erlang cluster"}]),
    prometheus_gauge:new([{name, cluster_size},
                          {help, "Current size of the erlang cluster"}]),
    prometheus_boolean:new([{name, cluster_green},
                            {help, "Whether or not the cluster is healthy"}]),
    prometheus_boolean:new([{name, cluster_leader},
                            {help, "Whether or not the node is the leader"}]),
    prometheus_boolean:new([{name, cluster_store_ready},
                            {help, "Whether or not the cluster store is initialized and ready"}]),
    prometheus_gauge:new([{name, cluster_store_subscriptions},
                          {help, "The number of subcriptions to the store"}]),
    prometheus_gauge:new([{name, cluster_store_size}, {help, "Size of the cluster store"}]),
    prometheus_counter:new([{name, cluster_store_partitions},
                            {help, "Count of the number of network partitions"}]),
    prometheus_counter:new([{name, cluster_leader_elections},
                            {help, "Count of the number of leader elections"}]),

    set(cluster_expected_size, length(cluster:members())),
    set(cluster_size, length([node() | nodes()])),
    set(cluster_green, cluster:state() =:= green),
    set(cluster_leader, cluster:is_leader()),
    set(cluster_store_ready, cluster_store:is_ready()),
    ok.

inc(Metric) ->
    prometheus_counter:inc(Metric).

set(Metric, Value) when is_integer(Value) ->
    prometheus_gauge:set(Metric, Value);
set(Metric, Value) when is_boolean(Value) ->
    prometheus_boolean:set(Metric, Value).

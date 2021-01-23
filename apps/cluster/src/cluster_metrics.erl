-module(cluster_metrics).

-export([register_metrics/0, inc/1, set/2]).

register_metrics() ->
    prometheus_gauge:new([{name, cluster_expected_size},
                          {help, "expected size of the cluster"}]),
    prometheus_gauge:new([{name, cluster_size}, {help, "current size of the cluster"}]),
    prometheus_boolean:new([{name, cluster_green}, {help, "is the cluster is healthy?"}]),
    prometheus_boolean:new([{name, cluster_leader}, {help, "is the node is the leader?"}]),
    prometheus_boolean:new([{name, cluster_store_ready},
                            {help, "is the cluster store ready?"}]),
    prometheus_gauge:new([{name, cluster_store_subscriptions},
                          {help, "the number of subcriptions to the store"}]),
    prometheus_gauge:new([{name, cluster_store_size}, {help, "size of the cluster store"}]),
    prometheus_counter:new([{name, cluster_store_partitions},
                            {help, "total number of the cluster store partitions"}]),
    prometheus_counter:new([{name, cluster_leader_elections},
                            {help, "total number of leader elections"}]),

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

# Clusterl: Simple clustering with Erlang and Kubernetes

Clusterl is a demo for `cluster`, the underlying OTP application
that makes it easy to turn your Erlang backend into a distributed 
system running inside Kubernetes using **statefulsets**.

## Usage

Add this dependency to your `rebar3.config`:

```
{cluster, {git, "https://github.com/pedro-gutierrez/cluster", {branch, "main"}
```

Then make sure the `cluster` app is part of your release.


## Writing to the Key-Value store

To write a key:

```
cluster_store:write(<<"someKey">>, <<"someValue">>).
```

To read a key:

```
cluster_store:read(<<"someKey">>>).
```

## Live Demo

Available [Here](http://cluster-pedro-gutierrez.cloud.okteto.net).

## Configuration

`cluster` is designed to work inside Kubernetes using **statefulsets**. If you are
not using statefulset, this library won't work. 

| Env variable | Description |
| --- | --- |
| `CLUSTER_SIZE` | the size of your cluster, eg: `2` |
| `CLUSTER_NAMESPACE` | your Kubernetes namespace |
| `CLUSTER_SERVICE` | the name of the Kubernetes headless service linked to your Statefulset |
| `ERLANG_COOKIE` | Erlang cookie for distribution |

Note: for now this library assumes a cluster of a fixed size. In the future, a dynamic 
cluster size might be supported.

## Features


- [x] Easy to embed as an extra application in your OTP release
- [x] Very simple configuration via environment variables
- [x] Uses Erlang distribution
- [x] Implements leader election for Active/Passive architectures 
- [x] Embedded and replicated in memory Key-Value store based on Mnesia
- [x] Detects network partitions
- [ ] Custom conflict resolution 
- [x] Both `automatic` and `manual` netplit recovery modes
- [ ] Exports Prometheus metrics
- [x] Cluster management via REST
- [ ] Key-Value store subscriptions

# Dependencies

This library has been writen in standard Erlang/OTP with minimal 
library dependencies: 

* Cowboy `2.0.0-pre.1` (web server)
* Prometheus `4.2.0` (monitoring)
* Jiffy `0.14.11"` (JSON library)
* Lagger `3.2.2"` (Logging)



## Clusterl: Simple clustering with Erlang and Kubernetes

Clusterl is a demo for `cluster`, the underlying OTP application
that makes it easy to turn your Erlang backend into a distributed 
system running inside Kubernetes using **statefulsets**.

## Usage

Add this dependency to your `rebar3.config`:

```
{cluster, {git, "https://github.com/pedro-gutierrez/cluster", {branch, "main"}
```

Then make sure the `cluster` app is part of your release.


## Live Demo

Available [Here](http://front-pedro-gutierrez.cloud.okteto.net).

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


* Easy to embed as an extra application in your OTP release
* Very simple configuration via environment variables
* Uses Erlang distribution
* Implements leader election of Active/Passive architectures 
* Embedded and replicated in memory KV store based on Mnesia
* Detects network partitions and allows for custom conflict resolution
* Both `automatic` and `manual` netplit recovery modes
* Exports Prometheus metrics
* REST api for cluster management and KV

# Dependencies

This library has been writen in standard Erlang/OTP with minimal 
library dependencies: 

* Cowboy (web server)
* Prometheus (monitoring)
* Jiffy (JSON library)
* Lagger (Logging)



#!/bin/sh
NODE_NAME="$CLUSTER_SERVICE@$(hostname).$CLUSTER_SERVICE.$CLUSTER_NAMESPACE.svc.cluster.local" _build/default/rel/clusterl/bin/clusterl console
shell:
	@CLUSTER_SERVICE=netcomp \
		CLUSTER_NAMESPACE=nc \
		CLUSTER_SIZE=2 \
		ERLANG_COOKIE=netcomp \
		bash -c "rebar3 shell \
		--config config/sys.config \
		--name netcomp@netcomp-0.netcomp.nc.svc.cluster.local"
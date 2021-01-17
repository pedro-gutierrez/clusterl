shell:
	@CLUSTER_SERVICE=netcomp \
		CLUSTER_NAMESPACE=nc \
		CLUSTER_SIZE=1 \
		ERLANG_COOKIE=netcomp \
		rebar3 shell \
		--config config/sys.config \
		--name netcomp@netcomp-0.netcomp.nc.svc.cluster.local
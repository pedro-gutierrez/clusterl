FROM erlang:22
RUN apt-get update
RUN apt-get install -y curl wget
ENV CODE_LOADING_MODE interactive
ENV RELX_REPLACE_OS_VARS true
RUN mkdir -p /tmp/mnesia
WORKDIR /opt/clusterl
ADD rebar.config .
ADD rebar3 .
ADD entrypoint.sh .
ADD config config
RUN ./rebar3 release
CMD ["/opt/clusterl/entrypoint.sh"]

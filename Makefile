REBAR3=/usr/local/bin/rebar3
BUILD_DIR=./_build/default/rel/dss
INSTALL_DIR=/home/dss

clean:
	$(REBAR3) clean

tar:
	$(REBAR3) tar

test:
	$(REBAR3) eunit

compile:
	$(REBAR3) compile;
	$(REBAR3) release

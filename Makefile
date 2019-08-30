REBAR3=/usr/local/bin/rebar3
BUILD_DIR=./_build/default/rel/dss
INSTALL_DIR=/home/dss

clean:
	$(REBAR3) clean

tar:
	$(REBAR3) tar

check:
	$(REBAR3) ct

compile:
	$(REBAR3) compile;
	$(REBAR3) release

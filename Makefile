REBAR=`which rebar`

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

.PHONY: test

test: ct

ct:
	@$(REBAR) skip_deps=true ct

PROJECT = jamdb_sybase

include erlang.mk

clean-all: clean
	@rm -rf logs

CT_OPTS = -cover test/cover.spec

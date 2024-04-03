REBAR=rebar3
APP=erlang-tools

EUNIT_ARGS=
CT_ARGS=


all: compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

docs:
	$(REBAR) as docs edoc

xref:
	$(REBAR) xref

check: test itest xref

test: eunit
eunit:
	$(REBAR) eunit --verbose $(EUNIT_ARGS)

itest: ct
ct:
	$(REBAR) ct --verbose $(CT_ARGS)

rtest:
	$(REBAR) as test shell --script test/$(APP)_RTEST.escript --name "$(APP)@`hostname --fqdn`"

shell:
	$(REBAR) shell --apps lager --config test/sys.config

clean:
	$(REBAR) clean

clean-all:
	$(REBAR) clean --all

.PHONY: all deps compile docs xref check test eunit itest ct rtest shell clean clean-all

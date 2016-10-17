ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
APPNAME = deliverly
ERL_LIBS:=apps:deps


ERL=erl +A 4 +K true
ifeq (,$(wildcard ./rebar3))
	REBAR := $(shell which rebar3)
else
	REBAR := ./rebar3
endif

all: compile

update:
	git pull

compile:
	@$(REBAR) compile

release:
	@$(REBAR) as prod release

test: eunit ct

eunit:
	@$(REBAR) eunit

ct:
	@$(REBAR) ct

clean:
	@$(REBAR) clean

dialyze:
	dialyzer -r apps/deliverly/ebin/

run:
	@$(REBAR) shell

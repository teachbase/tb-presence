ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
APPNAME = deliverly
ERL_LIBS:=apps:deps


ERL=erl +A 4 +K true
ifeq (,$(wildcard ./rebar))
	REBAR := $(shell which rebar)
else
	REBAR := ./rebar
endif

all: compile

update:
	git pull

get_deps:
	@$(REBAR) get-deps

update_deps:
	@$(REBAR) update-deps

compile:
	@$(REBAR) compile

release: clean compile
	@$(REBAR) generate force=1

soft-release:
	@$(REBAR) generate force=1

test: eunit ct

eunit:
	@$(REBAR) skip_deps=true eunit

ct:
	@$(REBAR) skip_deps=true ct

clean:
	@$(REBAR) clean

dialyze:
	dialyzer -r apps/deliverly/ebin/

shell:
	ERL_LIBS=apps:deps erl -args_file files/vm.args -sasl errlog_type error -boot start_sasl -config files/app.config

run:
	ERL_LIBS=apps:deps erl -args_file files/vm.args -sasl errlog_type error -boot  start_sasl -sname de1@localhost -s $(APPNAME) -s tb_perf -s tb_visits -s tb_meetings -embedded -config files/app.config

run2:
	ERL_LIBS=apps:deps erl -args_file files/vm.args -sasl errlog_type error -boot  start_sasl -sname de2@localhost -s $(APPNAME) -s tb_perf -s tb_visits -deliverly config \"del2.config\" -embedded -config files/app.config




	
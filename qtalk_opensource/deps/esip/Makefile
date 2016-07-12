REBAR=./rebar

all: src

src:
	$(REBAR) compile

clean:
	$(REBAR) clean

test: all
	$(REBAR) -v skip_deps=true eunit

deps := $(wildcard deps/*/ebin)

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
	-o dialyzer/erlang.log --apps kernel stdlib erts; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/deps.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/deps.plt \
	-o dialyzer/deps.log $(deps); \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/esip.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/esip.plt \
	-o dialyzer/ejabberd.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

deps_plt: dialyzer/deps.plt
	@dialyzer --plt dialyzer/deps.plt --check_plt -o dialyzer/deps.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

esip_plt: dialyzer/esip.plt
	@dialyzer --plt dialyzer/esip.plt --check_plt -o dialyzer/ejabberd.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer: erlang_plt deps_plt esip_plt
	@dialyzer --plts dialyzer/*.plt --no_check_plt \
	--get_warnings -o dialyzer/error.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

check-syntax:
	gcc -o nul -S ${CHK_SOURCES}

.PHONY: clean src test all dialyzer erlang_plt deps_plt esip_plt

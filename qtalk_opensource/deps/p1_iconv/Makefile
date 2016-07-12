all: src

src:
	rebar compile

clean:
	rebar clean

test:
	rebar skip_deps=true eunit

.PHONY: clean src

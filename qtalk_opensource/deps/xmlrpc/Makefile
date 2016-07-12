ifeq ($(DEBUG),true)
DEBUG_FLAGS = -DDEBUG
else
DEBUG_FLAGS =
endif

XMLRPC_SOURCE_ROOT=.
XMLRPC_INCLUDE_DIR=$(XMLRPC_SOURCE_ROOT)/include

SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))

#other args: +native +"{hipe,[o3,verbose]}" -Ddebug=true +debug_info +no_strict_record_tests
ERLC_OPTS=-I $(XMLRPC_INCLUDE_DIR) -I $(INCLUDE_DIR) -o $(EBIN_DIR) $(DEBUG_FLAGS) -DTEST -Wall +debug_info
ERL_CALL=erl -pa $(EBIN_DIR)

all: $(EBIN_DIR) $(TARGETS)

$(EBIN_DIR):
	mkdir -p $@

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

run-tests: all
	$(ERL_CALL) \
	  -eval 'eunit:test("ebin", [verbose])' \
	  -s init stop

dialyze: $(TARGETS)
	dialyzer -c $?

clean:
	rm -f ebin/*.beam $(TARGETS)
	rm -f build-stamp install-stamp

distclean: clean


PROJECT = erl_finger_trees
include erlang.mk

# The included erlang.mk has the tests targets removed and instead implemented
# below. 'make tests' runs the PropEr tests.

TEST_DEPS = proper
dep_proper = git https://github.com/manopapad/proper.git master

# Test deps

ALL_TEST_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(TEST_DEPS))
$(foreach dep,$(TEST_DEPS),$(eval $(call dep_target,$(dep))))

build-test-deps: $(ALL_TEST_DEPS_DIRS)
	@for dep in $(ALL_TEST_DEPS_DIRS) ; do $(MAKE) -C $$dep; done

# Tests, general

TEST_DIR = test

TEST_ERLC_OPTS ?= +debug_info +warn_export_vars +warn_shadow_vars \
	+warn_obsolete_guard -DTEST=1 -DEXTRA=1

tests: ERLC_OPTS = $(TEST_ERLC_OPTS)
tests:: clean deps app build-tests

clean:: clean-tests

build-tests: build-test-deps
	$(gen_verbose) erlc -v $(TEST_ERLC_OPTS) -I include/ -o $(TEST_DIR) \
		$(wildcard $(TEST_DIR)/*.erl $(TEST_DIR)/*/*.erl) -pa ebin/

clean-tests:
	$(gen_verbose) rm -rf test/*.beam

# PropEr

PROPER_OPTS ?= 
PROPER_MODS ?= $(basename $(notdir $(wildcard $(TEST_DIR)/*_proper.erl)))

PROPER_RUN = erl \
	-no_auto_compile \
	-pa $(realpath $(TEST_DIR)) $(DEPS_DIR)/*/ebin \
	-pz $(realpath ebin) \
	-eval 'case lists:all(fun (M) -> \
	                          case proper:module(M, [$(PROPER_OPTS)]) of \
	                              {error, Reason} -> \
	                                  io:format("~p", [Reason]), \
	                                  false; \
	                              [] -> \
	                                  true; \
	                              Ok -> \
	                                  io:format("~p", [Ok]), \
	                                  true \
	                          end \
	                      end, \
	                      [$(shell echo $(PROPER_MODS) | sed "s/ /,/g")]) of \
	           true -> halt(0); \
	           false -> halt(1) \
	       end.'

proper:
	$(gen_verbose) $(PROPER_RUN)

tests:: proper

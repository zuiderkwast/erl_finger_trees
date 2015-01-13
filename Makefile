PROJECT = erl_finger_trees

DEPS = parse_trans
dep_parse_trans = git https://github.com/uwiger/parse_trans.git 2.9.1

TEST_DEPS = proper
dep_proper = git https://github.com/manopapad/proper.git master

include erlang.mk

# Tests using PropEr

# PropEr is downloaded only when 'make test' is called.


tests:: proper

PROPER_OPTS ?=
PROPER_MODS ?= $(basename $(notdir $(wildcard $(TEST_DIR)/*_proper.erl)))

PROPER_RUN = erl \
	-no_auto_compile \
	-pa $(realpath $(TEST_DIR)) $(DEPS_DIR)/*/ebin \
	-pz $(realpath ebin) \
	-eval 'case lists:all(fun (M) -> \
	                          case proper:module(M, [$(PROPER_OPTS)]) of \
	                              {error, Reason} -> \
	                                  io:format("FAIL: ~p", [Reason]), \
	                                  false; \
	                              [] -> \
	                                  true; \
	                              Ok -> \
	                                  io:format("OK: ~p", [Ok]), \
	                                  true \
	                          end \
	                      end, \
	                      [$(shell echo $(PROPER_MODS) | sed "s/ /,/g")]) of \
	           true -> halt(0); \
	           false -> halt(1) \
	       end.'

proper:
	$(gen_verbose) $(PROPER_RUN)

-module(sequence_pt_tests).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, sequence_pt}).

empty_test() ->
    ?assert(sequence:is_empty(<<[]:sequence>>)).

nonempty_test() ->
    ?assertEqual(
        [a, aw,vw, wve, 32, 42,
         bwr, sdsd, a, a, a, a,
         foo, wd, awef, awd, we,
         wefaw, sfwe, sf, 23, f23,
         "sfwef", sf],
        sequence:to_list(<<[a, aw,vw, wve, 32, 42,
                            bwr, sdsd, a, a, a, a,
                            foo, wd, awef, awd, we,
                            wefaw, sfwe, sf, 23, f23,
                            "sfwef", sf]:sequence>>)),
    ?assertEqual([1, 2, 3], sequence:to_list(<<[1, 2, 3]:sequence>>)),
    ?assertEqual([42], sequence:to_list(<<[42]:sequence>>)),
    ?assertEqual([], sequence:to_list(<<[]:sequence>>)).


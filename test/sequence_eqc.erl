-module(sequence_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

non_empty_vec(G) ->
    ?SIZED(Size, well_defined(non_empty_vec(Size, G))).
non_empty_vec(N, G) ->
    ?SUCHTHAT(V, vec(N, G), sequence:is_empty(eval(V)) == false).

vec(G) ->
    ?SIZED(Size, well_defined(vec(Size, G))).

vec(0, G) ->
    frequency([{1, {call, sequence, new, []}},
            {2, ?LAZY({call, lists, foldl, [return(fun sequence:pushr/2),
                            {call, sequence, new, []}, list(G)]})},
            {2, ?LAZY({call, lists, foldl, [return(fun sequence:pushl/2),
                            {call, sequence, new, []}, list(G)]})}]);
vec(N, G) ->
    frequency([{3, vec(0, G)},
            {2, ?LAZY(?LETSHRINK([V], [vec(N-1, G)],
                        {call, sequence, pushl, [G, V]}))},
            {2, ?LAZY(?LETSHRINK([V], [vec(N-1, G)],
                        {call, sequence, pushr, [G, V]}))},
            {3, ?LAZY(?LETSHRINK([V1, V2],
                        [vec(N div 2, G), vec(N div 2, G)],
                        {call, sequence, concat, [V1, V2]}))},
            {1, ?LAZY(?LETSHRINK([V], [non_empty_vec(N-1, G)],
                        {call, sequence, popl, [V]}))},
            {1, ?LAZY(?LETSHRINK([V], [non_empty_vec(N-1, G)],
                        {call, sequence, popr, [V]}))},
            {1, ?LAZY(?LET({V, L, H},
                        ?LET({V2, L2},
                            ?LET(V1, non_empty_vec(N-1, G),
                                {return(V1), choose(1, sequence:size(eval(V1)))}),
                            {return(V2), return(L2),
                                choose(L2, sequence:size(eval(V2)))}),
                        {call, sequence, subvec, [L, H, V]}))}]).

model(V) ->
    model(V, []).

model(V, L) ->
    case sequence:is_empty(V) of
        true -> L;
        false -> model(sequence:popr(V), [sequence:peekr(V) | L])
    end.

mpushl(X, L) ->
    [X | L].

mpeekl([H | _T]) ->
    H.

mpopl([_H | T]) ->
    T.

mpushr(X, L) ->
    L ++ [X].

mpeekr(L) ->
    lists:last(L).

mpopr([_]) ->
    [];
mpopr([H | T]) ->
    [H | mpopr(T)].

mconcat(L1, L2) ->
    L1 ++ L2.

msize(L) ->
    length(L).

nthhead(0, _) ->
    [];
nthhead(N, [X | L]) ->
    [X | nthhead(N - 1, L)].

msubvec(N, M, L) ->
    lists:nthtail(N - 1, nthhead(M, L)).

mnth(N, L) ->
    lists:nth(N, L).

minsert(N, X, L) ->
    nthhead(N - 1, L) ++ [X | lists:nthtail(N - 1, L)].

mreplace(N, X, L) ->
    nthhead(N - 1, L) ++ [X | lists:nthtail(N, L)].

prop_pushl() -> prop_pushl(int()).
prop_pushl(G) ->
    ?FORALL({E, V}, {G, vec(G)},
        mpushl(E, model(eval(V))) == model(sequence:pushl(E, eval(V)))).

prop_peekl() -> prop_peekl(int()).
prop_peekl(G) ->
    ?FORALL(V, non_empty_vec(G),
        mpeekl(model(eval(V))) == sequence:peekl(eval(V))).

prop_popl() -> prop_popl(int()).
prop_popl(G) ->
    ?FORALL(V, non_empty_vec(G),
        mpopl(model(eval(V))) == model(sequence:popl(eval(V)))).

prop_pushr() -> prop_pushr(int()).
prop_pushr(G) ->
    ?FORALL({E, V}, {G, vec(G)},
        mpushr(E, model(eval(V))) == model(sequence:pushr(E, eval(V)))).

prop_peekr() -> prop_peekr(int()).
prop_peekr(G) ->
    ?FORALL(V, non_empty_vec(G),
        mpeekr(model(eval(V))) == sequence:peekr(eval(V))).

prop_popr() -> prop_popr(int()).
prop_popr(G) ->
    ?FORALL(V, non_empty_vec(G),
        mpopr(model(eval(V))) == model(sequence:popr(eval(V)))).

prop_concat() -> prop_concat(int()).
prop_concat(G) ->
    ?FORALL({V1, V2}, {vec(G), vec(G)},
        mconcat(model(eval(V1)), model(eval(V2))) ==
        model(sequence:concat(eval(V1), eval(V2)))).

prop_concat2() -> prop_concat2(int()).
prop_concat2(G) ->
    ?FORALL({L1, L2}, {long_list(G), long_list(G)},
        begin
            V1 = lists:foldl(fun sequence:pushr/2, sequence:new(), L1),
            V2 = lists:foldr(fun sequence:pushl/2, sequence:new(), L2),
            V12 = sequence:concat(V1, V2),
            L12 = L1 ++ L2,
            L12 == model(V12) andalso
                sequence:size(V12) == sequence:size(V1) + sequence:size(V2) andalso
                sequence:size(V12) == length(L12)
        end).

long_list(G) ->
    ?SIZED(Size, resize(Size * 5, list(G))).

prop_size() -> prop_size(int()).
prop_size(G) ->
    ?FORALL(V, vec(G),
        msize(model(eval(V))) == sequence:size(eval(V))).

list_cons(X, L) ->
    [X | L].

prop_subvec() -> prop_subvec(int()).
prop_subvec(G) ->
    ?FORALL({V, N, M},
        ?LET({V2, N},
            ?LET(V1, non_empty_vec(G),
                {return(V1), choose(1, sequence:size(eval(V1)))}),
            {return(V2), return(N), choose(N, sequence:size(eval(V2)))}),
        msubvec(N, M, model(eval(V))) == model(sequence:subvec(N, M, eval(V)))).

prop_nth() -> prop_nth(int()).
prop_nth(G) ->
    ?FORALL({V, N},
        ?LET(V1, non_empty_vec(G),
            {return(V1), choose(1, sequence:size(eval(V1)))}),
        mnth(N, model(eval(V))) == sequence:nth(N, eval(V))).

prop_insert() -> prop_insert(int()).
prop_insert(G) ->
    ?FORALL({V, N, X},
        ?LET(V1, vec(G),
            {return(V1), choose(1, max(1, sequence:size(eval(V1)))), G}),
        minsert(N, X, model(eval(V))) == model(sequence:insert(N, X, eval(V)))).

prop_replace() -> prop_replace(int()).
prop_replace(G) ->
    ?FORALL({V, N, X},
        ?LET(V1, non_empty_vec(G),
            {return(V1), choose(1, sequence:size(eval(V1))), G}),
        mreplace(N, X, model(eval(V))) == model(sequence:replace(N, X, eval(V)))).

prop_foldl() -> prop_foldl(int()).
prop_foldl(G) ->
    ?FORALL(V, vec(G),
        lists:reverse(model(eval(V))) =:=
        sequence:foldl(fun list_cons/2, [], eval(V))).

prop_foldr() -> prop_foldr(int()).
prop_foldr(G) ->
    ?FORALL(V, vec(G),
        model(eval(V)) =:= sequence:foldr(fun list_cons/2, [], eval(V))).

prop_from_list() -> prop_from_list(int()).
prop_from_list(G) ->
    ?FORALL(L, list(G),
        L == model(sequence:from_list(L))).

prop_to_list() -> prop_to_list(int()).
prop_to_list(G) ->
    ?FORALL(V, vec(G),
        model(eval(V)) == sequence:to_list(eval(V))).

prop_build_unbuild() -> prop_build_unbuild(int()).
prop_build_unbuild(G) ->
    ?FORALL(L, list(G),
        L =:= sequence:foldl(fun list_cons/2, [],
            lists:foldl(fun sequence:pushl/2, sequence:new(), L))).

prop_build_unbuild2() -> prop_build_unbuild2(int()).
prop_build_unbuild2(G) ->
    ?FORALL(L, list(G),
        L =:= sequence:foldr(fun list_cons/2, [],
            lists:foldl(fun sequence:pushr/2, sequence:new(), L))).

prop_build_unbuild3() -> prop_build_unbuild3(int()).
prop_build_unbuild3(G) ->
    ?FORALL(L, list(G),
        L =:= model(lists:foldl(fun sequence:pushr/2, sequence:new(), L))).

prop_build_unbuild4() -> prop_build_unbuild4(int()).
prop_build_unbuild4(G) ->
    ?FORALL({L1, L2}, {list(G), list(G)},
        L1 ++ L2 =:= model(sequence:concat(
                lists:foldr(fun sequence:pushl/2, sequence:new(), L1),
                lists:foldr(fun sequence:pushl/2, sequence:new(), L2)))).

prop_build_unbuild5() -> prop_build_unbuild5(int()).
prop_build_unbuild5(G) ->
    ?FORALL({L1, L2}, {list(G), list(G)},
        L1 ++ L2 =:= model(sequence:concat(
                lists:foldr(fun sequence:pushl/2, sequence:new(), L1),
                lists:foldl(fun sequence:pushr/2, sequence:new(), L2)))).

prop_build_unbuild6() -> prop_build_unbuild6(int()).
prop_build_unbuild6(G) ->
    ?FORALL({L1, L2}, {list(G), list(G)},
        L1 ++ L2 =:= model(sequence:concat(
                lists:foldl(fun sequence:pushr/2, sequence:new(), L1),
                lists:foldr(fun sequence:pushl/2, sequence:new(), L2)))).

prop_build_unbuild7() -> prop_build_unbuild7(int()).
prop_build_unbuild7(G) ->
    ?FORALL({L1, L2}, {list(G), list(G)},
        L1 ++ L2 =:= model(sequence:concat(
                lists:foldl(fun sequence:pushr/2, sequence:new(), L1),
                lists:foldl(fun sequence:pushr/2, sequence:new(), L2)))).

%%
%% Copyright (c) 2010, Gregory Rogers All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(sequence).

-export([new/0, is_empty/1, size/1,
        pushl/2, peekl/1, popl/1,
        pushr/2, peekr/1, popr/1,
        concat/2, subvec/3,
        nth/2, insert/3, replace/3,
        foldl/3, foldr/3,
        to_list/1, from_list/1]).

-export_type([sequence/0, sequence/1]).

-type sequence(X) :: {sequence, finger_tree:finger_tree(integer(), X)}.
-type sequence() :: sequence(any()).

% size monoid
size_meas(_) ->
    1.

-define(size_id, 0).

size_op(A, B) ->
    A + B.

-spec new() -> sequence().
new() ->
    {sequence, finger_tree:new(fun size_meas/1, ?size_id, fun size_op/2)}.

-spec is_empty(sequence()) -> boolean().
is_empty({sequence, FT}) ->
    finger_tree:is_empty(FT).

-spec size(sequence()) -> integer().
size({sequence, FT}) ->
    finger_tree:measure(FT).

-spec pushl(X, Seq::sequence(X)) -> sequence(X).
pushl(X, {sequence, FT}) ->
    {sequence, finger_tree:pushl(X, FT)}.

-spec peekl(Seq::sequence(X)) -> X.
peekl({sequence, FT}) ->
    finger_tree:peekl(FT).

-spec popl(Seq::sequence(X)) -> sequence(X).
popl({sequence, FT}) ->
    {sequence, finger_tree:popl(FT)}.

-spec pushr(X, Seq::sequence(X)) -> sequence(X).
pushr(X, {sequence, FT}) ->
    {sequence, finger_tree:pushr(X, FT)}.

-spec peekr(Seq::sequence(X)) -> X.
peekr({sequence, FT}) ->
    finger_tree:peekr(FT).

-spec popr(Seq::sequence(X)) -> sequence(X).
popr({sequence, FT}) ->
    {sequence, finger_tree:popr(FT)}.

-spec concat(Seq1::sequence(X), Seq2::sequence(X)) -> sequence(X).
concat({sequence, FT1}, {sequence, FT2}) ->
    {sequence, finger_tree:concat(FT1, FT2)}.

-spec subvec(integer(), integer(), Seq::sequence(X)) -> sequence(X).
subvec(L, H, {sequence, FT} = V) when L >= 1, L =< H ->
    subvec(L, H, finger_tree:measure(FT), V).

subvec(1, Sz, Sz, {sequence, FT}) ->
    {sequence, FT};
subvec(1, H, _Sz, {sequence, FT}) ->
    {sequence, finger_tree:takewhile(fun(I) -> I =< H end, FT)};
subvec(L, Sz, Sz, {sequence, FT}) ->
    {sequence, finger_tree:dropwhile(fun(I) -> I < L end, FT)};
subvec(L, H, _Sz, {sequence, FT}) ->
    FT1 = finger_tree:takewhile(fun(I) -> I =< H end, FT),
    {sequence, finger_tree:dropwhile(fun(I) -> I < L end, FT1)}.

-spec nth(integer(), Seq::sequence(X)) -> X.
nth(N, {sequence, FT}) ->
    FT1 = finger_tree:dropwhile(fun(I) -> I < N end, FT),
    finger_tree:peekl(FT1).

%% @doc Insert the element such that it becomes the Nth element
-spec insert(integer(), X, Seq::sequence(X)) -> sequence(X).
insert(N, X, {sequence, FT}) ->
    {L, R1} = finger_tree:split(fun(I) -> I < N end, FT),
    R2 = finger_tree:pushl(X, R1),
    {sequence, finger_tree:concat(L, R2)}.

-spec replace(integer(), X, Seq::sequence(X)) -> sequence(X).
replace(N, X, {sequence, FT}) ->
    {L, R1} = finger_tree:split(fun(I) -> I < N end, FT),
    R2 = finger_tree:popl(R1),
    R3 = finger_tree:pushl(X, R2),
    {sequence, finger_tree:concat(L, R3)}.

-spec foldl(fun((X, A) -> A), A, Seq::sequence(X)) -> A.
foldl(Fun, Acc, {sequence, FT}) ->
    finger_tree:foldl(Fun, Acc, FT).

-spec foldr(fun((X, A) -> A), A, Seq::sequence(X)) -> A.
foldr(Fun, Acc, {sequence, FT}) ->
    finger_tree:foldr(Fun, Acc, FT).

-spec to_list(Seq::sequence(X)) -> [X].
to_list({sequence, FT}) ->
    finger_tree:foldr(fun(X, L) -> [X | L] end, [], FT).

-spec from_list(Xs::[X]) -> sequence(X).
from_list(L) ->
    {sequence, lists:foldl(fun finger_tree:pushr/2,
            finger_tree:new(fun size_meas/1, ?size_id, fun size_op/2), L)}.

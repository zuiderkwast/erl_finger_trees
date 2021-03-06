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

-module(prio_queue).

-export([new/0, new/1, is_empty/1, push/2, pop/1]).
-export_type([prio_queue/0, prio_queue/1]).

-type prio_queue(X) :: {prio_queue, cmpfun(X),
                        finger_tree:finger_tree(maybe(X), X)}.
-type prio_queue() :: prio_queue(any()).
-type cmpfun(X) :: fun((X, X) -> boolean()).
-type maybe(X) :: {just, X} | nothing.


% prio monoid
prio_meas(X) ->
    {just, X}.

-define(prio_id, nothing).

make_prio_op(CmpFun) ->
    fun(MX, MY) ->
            case {MX, MY} of
                {nothing, Y} -> Y;
                {X, nothing} -> X;
                {{just, X}, {just, Y}} ->
                    case CmpFun(X, Y) of
                        true -> {just, X};
                        _ -> {just, Y}
                    end
            end
    end.

greater(X, Y) ->
    X > Y.

%% @doc Creates a max-priority queue.
-spec new() -> prio_queue().
new() ->
    {prio_queue, fun greater/2,
        finger_tree:new(fun prio_meas/1, ?prio_id,
            make_prio_op(fun greater/2))}.

%% @doc Creates a priority queue with a custom priority function.
%% CmpFun is a function that returns true if the first argument has a
%% higher priority than the second argument, and false otherwise. If CmpFun
%% behaves like `<', it is a min priority queue, if it that behaves like `>',
%% it is a max priority queue.
-spec new(cmpfun(X)) -> prio_queue(X).
new(CmpFun) ->
    {prio_queue, CmpFun,
        finger_tree:new(fun prio_meas/1, ?prio_id, make_prio_op(CmpFun))}.

-spec is_empty(prio_queue()) -> boolean().
is_empty({prio_queue, _CmpFun, FT}) ->
    finger_tree:is_empty(FT).

-spec push(X, prio_queue(X)) -> prio_queue(X).
push(X, {prio_queue, CmpFun, FT}) ->
    {prio_queue, CmpFun, finger_tree:pushr(X, FT)}.

-spec pop(prio_queue(X)) -> {X, prio_queue(X)}.
pop({prio_queue, CmpFun, FT}) ->
    MM = finger_tree:measure(FT),
    SplitFun = fun(MI) ->
            case {MM, MI} of
                {nothing, _I} -> false;
                {_M, nothing} -> true;
                {{just, M}, {just, I}} -> CmpFun(M, I)
            end
    end,
    {LFT, RFT} = finger_tree:split(SplitFun, FT),
    X = finger_tree:peekl(RFT),
    {X, {prio_queue, CmpFun, finger_tree:concat(LFT, finger_tree:popl(RFT))}}.

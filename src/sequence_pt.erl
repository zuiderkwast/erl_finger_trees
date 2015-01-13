%% @doc This parse transform adds the syntax `<<[...]:sequence>>' for
%% constructing sequences at compile time.
%%
%% TODO: Capture this syntax instead: `sequence:from_list([...])'
-module(sequence_pt).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    parse_trans:plain_transform(fun do_transform/1, Forms).

do_transform({bin, Line, [{bin_element, _, {nil, _}, {atom, _, sequence},
                           default}]}) ->
    %% <<[]:sequence>>
    make_abs_sequence([], Line);
do_transform({bin, Line, [{bin_element, _, {cons, _, _H, _T} = Cons,
                           {atom, _, sequence}, default}]}) ->
    %% <<[_, ...]:sequence>>
    Forms = abs_list_to_list_of_abs(Cons, []),
    %% Recursively apply transform to the elements of the list.
    Forms1 = [begin
                  [F] = parse_trans:plain_transform(fun do_transform/1, [Form]),
                  F
              end || Form <- Forms],
    %% Construct an abstract form for a sequence tree structure.
    make_abs_sequence(Forms1, Line);
do_transform(_) ->
    continue.

abs_list_to_list_of_abs({cons, _Line, H, T}, Acc) ->
    abs_list_to_list_of_abs(T, [H | Acc]);
abs_list_to_list_of_abs({nil, _Line}, Acc) ->
    lists:reverse(Acc).

make_abs_sequence(AbstractForms, Line) ->
    Length = length(AbstractForms),
    PlSeq = sequence:from_list(lists:duplicate(Length, placeholder)),
    PlSeqUnwrapped = unwrap_sequence(PlSeq),
    AbsPlSeqUnwrapped = erl_parse:abstract(PlSeqUnwrapped, [{line, Line}]),
    {AbsSeqUnwrapped, []} = replace_placeholders(AbsPlSeqUnwrapped, AbstractForms),
    wrap_abs_sequence(AbsSeqUnwrapped, Line).

%% Strips the outer tuples because they contain funs that can't be converted to
%% abstract form.
unwrap_sequence({sequence, {finger_tree, {_MeasFun, 0, _OpFun}, FT}}) ->
    FT.

%% @doc Returns an abstract form for the reverse of the function
%% `unwrap_sequence/1' inlined, e.g. abstract form for this:
%%
%% ```begin
%%     {sequence, {finger_tree, Meas, _}} = sequence:new(),
%%     {sequence, {finger_tree, Meas, FT}}
%% end'''
%%
%% (Another idea is to return an abstract form for a function call such as
%% `sequence_parse_trans:wrap_sequence(FT)' where the function is implemented
%% as the lines above.)
wrap_abs_sequence(AbsFT, Line) ->
    %{call, Line,
    %       {remote, Line,
    %                {atom, Line, sequence_parse_trans},
    %                {atom, Line, wrap_sequence}},
    %       [AbsFT]}.
    {block, Line,
        [{match, Line,
            {tuple, Line,
                [{atom, Line, sequence},
                 {tuple, Line,
                    [{atom, Line, finger_tree},
                     {var, Line, 'Meas'},
                     {var, Line, '_'}]}]},
            {call, Line,
                {remote, Line, {atom, Line, sequence}, {atom, Line, new}}, []}},
         {tuple, Line,
            [{atom, Line, sequence},
             {tuple, Line,
                [{atom, Line, finger_tree}, {var, Line, 'Meas'}, AbsFT]}]}]}.

%% Replace integer placeholders in a nested form representing a finger tree.
%% The placeholders are forms representing the atom 'placeholder'.
-spec replace_placeholders(tuple(), list()) -> {tuple(), list()}.
replace_placeholders({tuple, Line, Elements}, Replacements) ->
    {RevSubstElements, RemainingReplacements} = lists:foldl(
        fun
            ({atom, _, placeholder}, {Acc, [R | Rs]}) ->
                {[R | Acc], Rs};
            (Tuple = {tuple, _, _}, {Acc, Rs}) ->
                %% tree node; recursion
                {Tuple1, Rs1} = replace_placeholders(Tuple, Rs),
                {[Tuple1 | Acc], Rs1};
            (Node = {T, _, _}, {Acc, Rs}) when T == atom; T == integer ->
                %% tag or measure; unchanged
                {[Node | Acc], Rs}
        end,
        {[], Replacements},
        Elements
    ),
    {{tuple, Line, lists:reverse(RevSubstElements)}, RemainingReplacements}.

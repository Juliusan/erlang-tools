%%%
%%%
%%%
-module(erlang_tools_display_term_erl).
-compile([{parse_transform, lager_transform}]).
-export([to_binary/1, to_binary/2]).

%%
%%
%%
-ignore_xref([
    {?MODULE, to_binary, 1},    % Can be called by external users
    {?MODULE, to_binary, 2}     % Can be called by external users
]).


%%
%%  Used to address specific parts of Erlang term. E.g.:
%%    * [2] matches second element of any list/tuple: [a, this, b, c] or {a, this, b}.
%%    * [key] matches map entry with key 'key': #{a => b, key => this}.
%%    * [3, 2, key, 1] if term is a list/tuple, which third element is a list/tuple,
%%      which second element is a map, which has entry with 'key' as a key, which
%%      has a list/tuple as a value, then matches the first element of this list/tuple:
%%      [a, [b,c], {d, #{e => f, key => [this, g, h, i]}, j}].
%%    * [2, [1, 3]] if term is a list/tuple, which second element is list/tuple,
%%      then matches first and third elements of this list/tuple.
%%    * [2, '*'] if term is a list/tuple, which second element is list/tuple,
%%      then matches all the elements of this list/tuple.
%%    * [2, {1, 3}] if term is a list/tuple, which second element is list/tuple,
%%      then matches first, second and third elements of this list/tuple.
%%
-type term_path() ::
    [ term_path_segment() ].

-type term_path_segment() ::
    term_path_segment_element() |
    [ Element :: term_path_segment_element() ].    % Matches if name matches at least one Element

-type term_path_segment_element() ::
    Anything :: '*' |                                   % Matches any name
    Name     :: term_path_segment_element_name() |      % Matches only exactly the same name
    {                                                   % Matches any name between 'From' and 'To' (inclusive)
        From :: term_path_segment_element_name(),
        To   :: term_path_segment_element_name()
    }.

-type term_path_segment_element_name() ::
    Index  :: integer() |   % Used to name list or tuple elements
    MapKey :: term().       % Used to name map entries


%%% ============================================================================
%%% Public API.
%%% ============================================================================


%%
%%  Converts term to pretty formatted binary. All the list entries/tuple elements/
%%  map key-value pairs are displayed on a separate line with larger indentation
%%  than term, which contains the list/tuple/map. The exception is lists/tuples/maps,
%%  which contain exactly one element (the element is not displayed on a separate
%%  line) or none elements at all (displayed in a single line). Number of spaces,
%%  used in single indent, can be configured in `single_indent_length' application
%%  parameter.
%%  Optional `NoExpand' parameter may be used to list all the paths in the term,
%%  which should not be expanded to separate lines. All the paths listed in this
%%  parameter will be displayed on a single line, ignoring the amount of nested
%%  lists/tuples/maps.
%%  See: test cases in `to_binary_test_/0'
%%
-spec to_binary(
    Term :: term()
) ->
    binary().

to_binary(Term) ->
    to_binary(Term, []).

-spec to_binary(
    Term :: term(),
    NoExpands :: [ term_path() ]
) ->
    binary().

to_binary(Term, NoExpands) ->
    to_binary_indent(Term, 0, NoExpands).


%%% ============================================================================
%%% Private functions.
%%% ============================================================================


%%
%%
%%
to_binary_indent(Atom, _Indent, _NoExpands) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom);

to_binary_indent(Integer, _Indent, _NoExpands) when is_integer(Integer) ->
    erlang:integer_to_binary(Integer);

to_binary_indent(Float, _Indent, _NoExpands) when is_float(Float) ->
    erlang:float_to_binary(Float, [short]);

to_binary_indent(Binary, _Indent, _NoExpands) when is_binary(Binary) ->
    <<"<<\"", Binary/binary, "\">>">>;

to_binary_indent(List, Indent, NoExpands) when is_list(List) ->
    case {List, io_lib:printable_unicode_list(List)} of
        {[_|_], true} ->
            ListBin = erlang:list_to_binary(List),
            <<"\"", ListBin/binary, "\"">>;
        {_, _} ->
            IndexedList = lists:zip(lists:seq(1, erlang:length(List)), List),
            to_binary_indent_list(IndexedList, <<"[">>, <<"]">>, fun to_binary_indent/3, Indent, NoExpands)
    end;

to_binary_indent(Tuple, Indent, NoExpands) when is_tuple(Tuple) ->
    IndexedList = lists:zip(lists:seq(1, erlang:size(Tuple)), erlang:tuple_to_list(Tuple)),
    to_binary_indent_list(IndexedList, <<"{">>, <<"}">>, fun to_binary_indent/3, Indent, NoExpands);

to_binary_indent(Map, Indent, NoExpands) when is_map(Map) ->
    RecordToBinFun = fun({Key, Value}, I, NE) ->
        KeyBin = to_binary_indent(Key, I, NE),
        ValueBin = to_binary_indent(Value, I, NE),
        <<KeyBin/binary, " => ", ValueBin/binary>>
    end,
    IndexedList = [ {Key, {Key, Value}} || {Key, Value} <- lists:sort(maps:to_list(Map)) ],
    to_binary_indent_list(IndexedList, <<"#{">>, <<"}">>, RecordToBinFun, Indent, NoExpands).


%%
%%
%%
to_binary_indent_list(List, StartBin, EndBin, ElemToBinFun, Indent, NoExpands) ->
    DoExpand = case {erlang:length(List), NoExpands} of
        {N, _        } when N =< 1 -> false;
        {_, undefined}             -> false;
        {_, _        }             -> true
    end,
    case DoExpand of
        true ->
            IndentThisBin = get_indent(Indent),
            IndentNextBin = get_indent(Indent+1),
            ListConverted = erlang:iolist_to_binary(lists:join(<<",\n">>, lists:map(fun({Name, Elem}) ->
                NewNoExpands = filter_no_expands(Name, NoExpands),
                ElemBin = ElemToBinFun(Elem, Indent+1, NewNoExpands),
                <<IndentNextBin/binary, ElemBin/binary>>
            end, List))),
            <<StartBin/binary, "\n", ListConverted/binary, "\n", IndentThisBin/binary, EndBin/binary>>;
        false ->
            ListConverted = erlang:iolist_to_binary(lists:join(<<", ">>, lists:map(fun({Name, Elem}) ->
                NewNoExpands = filter_no_expands(Name, NoExpands),
                ElemToBinFun(Elem, Indent, NewNoExpands)
            end, List))),
            <<StartBin/binary, ListConverted/binary, EndBin/binary>>
    end.


%%
%%
%%
get_indent(Count) ->
    SingleIndentLength = erlang_tools:get_env(single_indent_length),
    Length = SingleIndentLength*Count,
    erlang:iolist_to_binary(lists:duplicate(Length, <<" ">>)).


%%
%%
%%
filter_no_expands(_Name, undefined) ->
    undefined;

filter_no_expands(Name, NoExpands) ->
    filter_no_expands(Name, NoExpands, []).

filter_no_expands(_Name, [], AccNoExpands) ->
    lists:reverse(AccNoExpands);

filter_no_expands(Name, [[Segment]|Others], AccNoExpands) ->
    case matches_no_expand_segment(Name, Segment) of
        true  -> undefined;
        false -> filter_no_expands(Name, Others, AccNoExpands)
    end;

filter_no_expands(Name, [[Segment|OtherSegments]|Others], AccNoExpands) ->
    NewAccNoExpands = case matches_no_expand_segment(Name, Segment) of
        true  -> [OtherSegments|AccNoExpands];
        false -> AccNoExpands
    end,
    filter_no_expands(Name, Others, NewAccNoExpands).


%%
%%
%%
matches_no_expand_segment(_Name, []     )                                      -> false;
matches_no_expand_segment(_Name, ['*'|_])                                      -> true;
matches_no_expand_segment(Name,  [Name|_])                                     -> true;
matches_no_expand_segment(Name,  [{From, To}|_]) when From =< Name, Name =< To -> true;
matches_no_expand_segment(Name,  [_|Others])                                   -> matches_no_expand_segment(Name, Others);
matches_no_expand_segment(Name,  NotAList)                                     -> matches_no_expand_segment(Name, [NotAList]).


%%% ============================================================================
%%% Test cases for internal functions.
%%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


%%
%%
%%
to_binary_test_() ->
    [
        ?_assertEqual(<<"alus">>, to_binary(alus)),
        ?_assertEqual(<<"123">>, to_binary(123)),
        ?_assertEqual(<<"123.456">>, to_binary(123.456)),
        ?_assertEqual(<<"<<\"alus\">>">>, to_binary(<<"alus">>)),
        ?_assertEqual(<<
            "[\n",
            "  321,\n",
            "  \"alus\",\n",
            "  [\n",
            "    alus,\n",
            "    456,\n",
            "    1.2,\n",
            "    \"sula\",\n",
            "    <<\"alus\">>,\n",
            "    {\n",
            "      555,\n",
            "      \"geras\"\n",
            "    }\n",
            "  ],\n",
            "  #{\n",
            "    a_pirmas => antras,\n",
            "    b_trecias => 123,\n",
            "    c_ketvirtas => [\n",
            "      a,\n",
            "      b,\n",
            "      c\n",
            "    ],\n",
            "    d_penktas => {\n",
            "      d,\n",
            "      e\n",
            "    }\n",
            "  },\n",
            "  2.3,\n",
            "  <<\"sula\">>,\n",
            "  sula\n",
            "]">>, to_binary([321, "alus", [alus, 456, 1.2, "sula", <<"alus">>, {555, "geras"}], #{a_pirmas => antras, b_trecias => 123, c_ketvirtas => [a,b,c], d_penktas => {d,e}}, 2.3, <<"sula">>, sula])),
        ?_assertEqual(<<"[[]]">>, to_binary([[]])),
        ?_assertEqual(<<"[{}]">>, to_binary([{}])),
        ?_assertEqual(<<"[#{}]">>, to_binary([#{}])),
        ?_assertEqual(<<"{[]}">>, to_binary({[]})),
        ?_assertEqual(<<"{{}}">>, to_binary({{}})),
        ?_assertEqual(<<"{#{}}">>, to_binary({#{}})),
        ?_assertEqual(<<"#{[] => []}">>, to_binary(#{[] => []})),
        ?_assertEqual(<<"#{{} => {}}">>, to_binary(#{{} => {}})),
        ?_assertEqual(<<"#{#{} => #{}}">>, to_binary(#{#{} => #{}})),
        ?_assertEqual(<<"[[alus]]">>, to_binary([[alus]])),
        ?_assertEqual(<<"{{2.3}}">>, to_binary({{2.3}})),
        ?_assertEqual(<<"#{\"petras\" => <<\"jonas\">>}">>, to_binary(#{"petras" => <<"jonas">>})),
        ?_assertEqual(<<"[[\n  alus,\n  123\n]]">>, to_binary([[alus, 123]])),
        ?_assertEqual(<<"{{\n  2.3,\n  \"labas\"\n}}">>, to_binary({{2.3, "labas"}})),
        ?_assertEqual(<<
            "{\n",
            "  a,\n",
            "  b,\n",
            "  {{{d}}},\n",
            "  c\n",
            "}">>, to_binary({a,b,{{{d}}},c})),
        ?_assertEqual(<<
            "{\n",
            "  a,\n",
            "  b,\n",
            "  {{{\n",
            "    d,\n",
            "    e,\n",
            "    f\n",
            "  }}},\n",
            "  c\n",
            "}">>, to_binary({a,b,{{{d,e,f}}},c})),
        ?_assertEqual(<<
            "[\n",
            "  a,\n",
            "  b,\n",
            "  [[[d]]],\n",
            "  c\n",
            "]">>, to_binary([a,b,[[[d]]],c])),
        ?_assertEqual(<<
            "[\n",
            "  a,\n",
            "  b,\n",
            "  [[[\n",
            "    d,\n",
            "    e,\n",
            "    f\n",
            "  ]]],\n",
            "  c\n",
            "]">>, to_binary([a,b,[[[d,e,f]]],c])),
        ?_assertEqual(<<
            "#{\n",
            "  a => b,\n",
            "  c => d,\n",
            "  e => #{f => #{g => #{h => i}}},\n",
            "  j => k\n",
            "}">>, to_binary(#{a => b, c => d, e => #{f => #{g => #{h => i}}}, j => k})),
        ?_assertEqual(<<
            "#{\n",
            "  a => b,\n",
            "  c => d,\n",
            "  e => #{f => #{g => #{\n",
            "    h => i,\n",
            "    j => k,\n",
            "    l => m\n",
            "  }}},\n",
            "  n => o\n",
            "}">>, to_binary(#{a => b, c => d, e => #{f => #{g => #{h => i, j => k, l => m}}}, n => o})),
        ?_assertEqual(<<"[\n  a,\n  b,\n  [c, d, e],\n  f\n]">>, to_binary([a, b, [c, d, e], f], [[3]])),
        ?_assertEqual(<<"[\n  a,\n  b,\n  [[c, d, e]],\n  f\n]">>, to_binary([a, b, [[c, d, e]], f], [[3,1]])),
        ?_assertEqual(<<
            "[\n",
            "  a,\n",
            "  [[c, d], [e, f, g]],\n",
            "  i,\n",
            "  [\n",
            "    [j, [k, l], m],\n",
            "    [\n",
            "      n,\n",
            "      o\n",
            "    ],\n",
            "    [p, r, s],\n",
            "    [t, u]\n",
            "  ]\n",
            "]">>, to_binary([a, [[c, d], [e, f, g]], i, [[j, [k, l], m], [n, o], [p, r, s], [t, u]]], [[2], [4, [1,{3,4}]]])),
        ?_assertEqual(<<
            "{\n",
            "  a,\n",
            "  {{c, d}, {e, f, g}},\n",
            "  i,\n",
            "  {\n",
            "    {j, {k, l}, m},\n",
            "    {\n",
            "      n,\n",
            "      o\n",
            "    },\n",
            "    {p, r, s},\n",
            "    {t, u}\n",
            "  }\n",
            "}">>, to_binary({a, {{c, d}, {e, f, g}}, i, {{j, {k, l}, m}, {n, o}, {p, r, s}, {t, u}}}, [[2], [4, [1,{3,4}]]])),
        ?_assertEqual(<<
            "#{\n",
            "  a => b,\n",
            "  p => r,\n",
            "  s => #{\n",
            "    t => [1, 2, 3],\n",
            "    u => [\n",
            "      2,\n",
            "      1\n",
            "    ],\n",
            "    v => [z, x, y]\n",
            "  },\n",
            "  #{c => d, e => #{f => f, g => h}} => #{i => j, k => #{l => m, n => o}}\n",
            "}">>, to_binary(#{a => b, #{c => d, e => #{f => f, g => h}} => #{i => j, k => #{l => m, n => o}},
             p => r, s => #{ t => [1, 2, 3], u => [2, 1], v => [z, x, y]}},
                [[#{c => d, e => #{f => f, g => h}}], [s, [t, v]]]))
    ].


%%
%%
%%
filter_no_expands_test_() ->
    [
        ?_assertEqual(undefined,                     filter_no_expands(1, [[2],[1],[3]])),
        ?_assertEqual(undefined,                     filter_no_expands(1, [[2,1,3],[1],[1,2,3]])),
        ?_assertEqual(undefined,                     filter_no_expands(1, [[1,2,3],[1],[2,1,3]])),
        ?_assertEqual([[2,3],[3,3],[1,1],[2,2],[4]], filter_no_expands(1, [[1,2,3],[2,1,3],['*',3,3],[{0,5},1,1],[[{3,4},1],2,2],[[{2,4},{0,1}],4]]))
    ].

-endif.

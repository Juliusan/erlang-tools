%%%
%%%
%%%
-module(erlang_tools_display_term_erl).
-compile([{parse_transform, lager_transform}]).
-export([to_binary/1]).

%%
%%
%%
-ignore_xref([
    {?MODULE, to_binary, 1}
]).



%%% ============================================================================
%%% Public API.
%%% ============================================================================


to_binary(Term) ->
    to_binary_indent(Term, 0).


%%% ============================================================================
%%% Private functions.
%%% ============================================================================


to_binary_indent(Atom, _Indent) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom);

to_binary_indent(Integer, _Indent) when is_integer(Integer) ->
    erlang:integer_to_binary(Integer);

to_binary_indent(Float, _Indent) when is_float(Float) ->
    erlang:float_to_binary(Float, [short]);

to_binary_indent(Binary, _Indent) when is_binary(Binary) ->
    <<"<<\"", Binary/binary, "\">>">>;

to_binary_indent(List, Indent) when is_list(List) ->
    case {List, io_lib:printable_unicode_list(List)} of
        {[_|_], true} ->
            ListBin = erlang:list_to_binary(List),
            <<"\"", ListBin/binary, "\"">>;
        {_, _} ->
            to_binary_indent_list(List, <<"[">>, <<"]">>, fun to_binary_indent/2, Indent)
    end;

to_binary_indent(Tuple, Indent) when is_tuple(Tuple) ->
    to_binary_indent_list(erlang:tuple_to_list(Tuple), <<"{">>, <<"}">>, fun to_binary_indent/2, Indent);

to_binary_indent(Map, Indent) when is_map(Map) ->
    RecordToBinFun = fun({Key, Value}, I) ->
        KeyBin = to_binary_indent(Key, I),
        ValueBin = to_binary_indent(Value, I),
        <<KeyBin/binary, " => ", ValueBin/binary>>
    end,
    to_binary_indent_list(lists:sort(maps:to_list(Map)), <<"#{">>, <<"}">>, RecordToBinFun, Indent).


to_binary_indent_list([], StartBin, EndBin, _ElemToBinFun, _Indent) ->
    <<StartBin/binary, EndBin/binary>>;

to_binary_indent_list([Elem], StartBin, EndBin, ElemToBinFun, Indent) ->
    ElemBin = ElemToBinFun(Elem, Indent),
    <<StartBin/binary, ElemBin/binary, EndBin/binary>>;

to_binary_indent_list(List = [_,_|_], StartBin, EndBin, ElemToBinFun, Indent) ->
    IndentThisBin = get_indent(Indent),
    IndentNextBin = get_indent(Indent+1),
    ListConverted = erlang:iolist_to_binary(lists:join(<<",\n">>, lists:map(fun(Elem) ->
        ElemBin = ElemToBinFun(Elem, Indent+1),
        <<IndentNextBin/binary, ElemBin/binary>>
    end, List))),
    <<StartBin/binary, "\n", ListConverted/binary, "\n", IndentThisBin/binary, EndBin/binary>>.


get_indent(Count) ->
    SingleIndentLength = erlang_tools:get_env(single_indent_length),
    Length = SingleIndentLength*Count,
    erlang:iolist_to_binary(lists:duplicate(Length, <<" ">>)).



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
            "}">>, to_binary(#{a => b, c => d, e => #{f => #{g => #{h => i, j => k, l => m}}}, n => o}))
    ].


-endif.

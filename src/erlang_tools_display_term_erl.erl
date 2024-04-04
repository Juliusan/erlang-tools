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
        {[], _} ->  % Normally, second element should be 'true'
            <<"[]">>;
        {_, true} ->
            ListBin = erlang:list_to_binary(List),
            <<"\"", ListBin/binary, "\"">>;
        {[Elem], false} ->
            ElemBin = to_binary_indent(Elem, Indent),
            <<"[", ElemBin/binary, "]">>;
        {[_,_|_], false} ->
            IndentThisBin = get_indent(Indent),
            IndentNextBin = get_indent(Indent+1),
            ListConverted = erlang:iolist_to_binary(lists:join(<<",\n">>, lists:map(fun(Elem) ->
                ElemBin = to_binary_indent(Elem, Indent+1),
                <<IndentNextBin/binary, ElemBin/binary>>
            end, List))),
            <<"[\n", ListConverted/binary, "\n", IndentThisBin/binary, "]">>
    end;

to_binary_indent(Tuple, Indent) when is_tuple(Tuple) ->
    case erlang:size(Tuple) of
        0 ->
            <<"{}">>;
        1 ->
            {Elem} = Tuple,
            ElemBin = to_binary_indent(Elem, Indent),
            <<"{", ElemBin/binary, "}">>;
        _ ->
            IndentThisBin = get_indent(Indent),
            IndentNextBin = get_indent(Indent+1),
            TupleConverted = erlang:iolist_to_binary(lists:join(<<",\n">>, lists:map(fun(Elem) ->
                ElemBin = to_binary_indent(Elem, Indent+1),
                <<IndentNextBin/binary, ElemBin/binary>>
            end, erlang:tuple_to_list(Tuple)))),
            <<"{\n", TupleConverted/binary, "\n", IndentThisBin/binary, "}">>
    end.


to_binary_indent_converted(TermBin, IndentLength) ->
    Indent = get_indent(IndentLength),
    <<Indent/binary, TermBin/binary>>.



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
            "  2.3,\n",
            "  <<\"sula\">>,\n",
            "  sula\n",
            "]">>, to_binary([321, "alus", [alus, 456, 1.2, "sula", <<"alus">>, {555, "geras"}], 2.3, <<"sula">>, sula])),
        ?_assertEqual(<<"[[]]">>, to_binary([[]])),
        ?_assertEqual(<<"[{}]">>, to_binary([{}])),
        ?_assertEqual(<<"{[]}">>, to_binary({[]})),
        ?_assertEqual(<<"{{}}">>, to_binary({{}})),
        ?_assertEqual(<<"[[alus]]">>, to_binary([[alus]])),
        ?_assertEqual(<<"[{123}]">>, to_binary([{123}])),
        ?_assertEqual(<<"{[\"labas\"]}">>, to_binary({["labas"]})),
        ?_assertEqual(<<"{{2.3}}">>, to_binary({{2.3}})),
        ?_assertEqual(<<"[[\n  alus,\n  123\n]]">>, to_binary([[alus, 123]])),
        ?_assertEqual(<<"[{\n  123,\n  \"labas\"\n}]">>, to_binary([{123, "labas"}])),
        ?_assertEqual(<<"{[\n  \"labas\",\n  2.3\n]}">>, to_binary({["labas", 2.3]})),
        ?_assertEqual(<<"{{\n  2.3,\n  alus\n}}">>, to_binary({{2.3, alus}})),
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
            "]">>, to_binary([a,b,[[[d,e,f]]],c]))
    ].


-endif.

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


to_binary_indent(Atom, IndentLength) when is_atom(Atom) ->
    to_binary_indent_converted(erlang:atom_to_binary(Atom), IndentLength);

to_binary_indent(Integer, IndentLength) when is_integer(Integer) ->
    to_binary_indent_converted(erlang:integer_to_binary(Integer), IndentLength);

to_binary_indent(Float, IndentLength) when is_float(Float) ->
    to_binary_indent_converted(erlang:float_to_binary(Float, [short]), IndentLength);

to_binary_indent(Binary, IndentLength) when is_binary(Binary) ->
    to_binary_indent_converted(<<"<<\"", Binary/binary, "\">>">>, IndentLength);

to_binary_indent(List, IndentLength) when is_list(List) ->
    case List of
        [] ->
            to_binary_indent_converted(<<"[]">>, IndentLength);
        [_|_] ->
            case io_lib:printable_unicode_list(List) of
                true ->
                    ListBin = erlang:list_to_binary(List),
                    to_binary_indent_converted(<<"\"", ListBin/binary, "\"">>, IndentLength);
                false ->
                    OpenConverted = to_binary_indent_converted(<<"[">>, IndentLength),
                    CloseConverted = to_binary_indent_converted(<<"]">>, IndentLength),
                    ListConverted = erlang:iolist_to_binary(lists:join(<<",\n">>, lists:map(fun(Elem) ->
                        to_binary_indent(Elem, IndentLength+1)
                    end, List))),
                    <<OpenConverted/binary, "\n", ListConverted/binary, "\n", CloseConverted/binary>>
            end
    end;

to_binary_indent(Tuple, IndentLength) when is_tuple(Tuple) ->
    case erlang:size(Tuple) of
        0 ->
            to_binary_indent_converted(<<"{}">>, IndentLength);
        1 ->
            {Elem} = Tuple,
            ElemBin = to_binary_indent(Elem, IndentLength),
            <<"{", ElemBin/binary, "}">>;
        _ ->
            OpenConverted = to_binary_indent_converted(<<"{">>, IndentLength),
            CloseConverted = to_binary_indent_converted(<<"}">>, IndentLength),
            ListConverted = erlang:iolist_to_binary(lists:join(<<",\n">>, lists:map(fun(Elem) ->
                to_binary_indent(Elem, IndentLength+1)
            end, erlang:tuple_to_list(Tuple)))),
            <<OpenConverted/binary, "\n", ListConverted/binary, "\n", CloseConverted/binary>>
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
            "]">>, to_binary([321, "alus", [alus, 456, 1.2, "sula", <<"alus">>, {555, "geras"}], 2.3, <<"sula">>, sula]))
    ].


-endif.

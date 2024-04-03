%%%
%%% TODO: Main API for the erlang-tools application.
%%%
-module(erlang_tools).
-behaviour(application).
-compile([{parse_transform, lager_transform}]).
-export([app/0]).
-export([start/0, get_env/1]).
-export([start/2, stop/1]).


%%
%%
%%
-ignore_xref([
    {?MODULE, start, 0}     % Initial entry point of the application
]).


%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%
%%
%%
app() ->
    erlang_tools.

%%
%%  Start this application explicitly.
%%
%%  This can be used from the command line, by providing `-s erlang_tools` for the `erl`.
%%
start() ->
    application:ensure_all_started(?MODULE:app(), permanent).


%%
%%  Get configuration parameters.
%%
get_env(Param) ->
    application:get_env(?MODULE:app(), Param, get_default(Param)).



%%% ============================================================================
%%% Callbacks for `application`.
%%% ============================================================================

%%
%%  Start the application.
%%
start(_StartType, _StartArgs) ->
    {ok, Pid} = erlang_tools_sup:start_link(),
    {ok, Pid, {}}.


%%
%%  Stop the application.
%%
stop({}) ->
    ok.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%
%%  Default values for configuration parameters
%%
get_default(single_indent_length) -> 2.

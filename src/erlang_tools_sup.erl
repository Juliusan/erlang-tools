%%%
%%% Main supervisor for the STATERA application.
%%%
-module(erlang_tools_sup).
-behaviour(supervisor).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0]).
-export([init/1]).


%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%
%%  Create this supervisor.
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).



%%% ============================================================================
%%% Callbacks for `supervisor`.
%%% ============================================================================

%%
%%  Supervisor initialization.
%%
init({}) ->
    SupFlags = #{
        strategy  => rest_for_one,
        intensity => 100,
        period    => 10
    },
    {ok, {SupFlags, []}}.

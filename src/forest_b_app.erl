%%%-------------------------------------------------------------------
%% @doc forest_b public API
%% @end
%%%-------------------------------------------------------------------

-module(forest_b_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    forest_b_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

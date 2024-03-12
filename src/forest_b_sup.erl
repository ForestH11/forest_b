%%%-------------------------------------------------------------------
%% @doc forest_b top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(forest_b_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [child(customer_request_server,worker),child(update_price_server,worker)],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
child(Module,Type) ->
    #{id => Module,
        start => {Module,start,[]},
        restart => permanent,
        shutdown => 2000,
        type => Type,
        modules => [Module]}.

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
                 intensity => 1,
                 period => 10},
    ChildSpecs = [
        % child(customer_request_server,crs1,worker),
        % child(customer_request_server,crs2,worker),
        % child(customer_request_server,worker),
        % child(update_price_server,worker)
        child(request_update_sup, supervisor),
        child(rrobin_serv, {worker,[crs1,crs2,crs3,crs4,crs5,crs6,crs7,crs8,crs9,crs10,crs11,crs12,crs13,crs14,crs15,crs16,crs17,crs18,crs19,crs20,crs21,crs22,crs23,crs24,crs25,crs26,crs27,crs28,crs29,crs30,crs31,crs32,crs33,crs34,crs35,crs36,crs37,crs38,crs39,crs40,crs41,crs42,crs43,crs44,crs45,crs46,crs47,crs48,crs49,crs50,crs51,crs52,crs53,crs54
]})
    ],
    {ok, {SupFlags, ChildSpecs}}.



%% internal functions
child(Module,{Type,Args}) ->
    #{id => Module,
        start => {Module,start,[Args]},
        restart => permanent,
        shutdown => 2000,
        type => Type,
        modules => [Module]};
child(Module,Type) ->
    #{id => Module,
        start => {Module,start,[]},
        restart => permanent,
        shutdown => 2000,
        type => Type,
        modules => [Module]}.
child(Module,Id,Type) ->
    #{id => Id,
        start => {Module,start,[local,Id,[]]},
        restart => permanent,
        shutdown => 2000,
        type => Type,
        modules => [Module]}.
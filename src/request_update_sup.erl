%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright © 2022, Lee S. Barney
%%% @reference Licensed under the 
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This is a template for supervisors that start up with no defined set of 
%%% OTP behaviors. Behaviors are then added and removed dynamically.
%%%
%%%
%%% @end

%%% Created : 24 October 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(request_update_sup).
-behaviour(supervisor).


%%%===================================================================
%%% Make sure to complete the documentation to match
%%% your supervisor's behavior and requirements.
%%%===================================================================

%%@private
-export([init/1]).
%%API functions
-export([start/0,start/1,start/3,add_child/4,remove_child/2]).

%%%===================================================================
%%% Public API functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% Used if there is only one such supervisor created,
%% it is registered locally under the module name,
%% and there is no startup information, such as an
%% initial list of children to start.
%%
%%
%% @end
%%--------------------------------------------------------------------
start()->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

%%--------------------------------------------------------------------
%% @doc
%%
%% Used if there is only one such supervisor created
%% and it is registered locally under the module name.
%%
%%
%% @end
%%--------------------------------------------------------------------
start(Start_info)->
    supervisor:start_link({local,?MODULE},?MODULE,Start_info).

%%--------------------------------------------------------------------
%% @doc
%%
%% Used if there can be many supervisors of this type
%% or if the supervisor is to be registered in any way 
%% but locally.
%%
%%
%% @end
%%--------------------------------------------------------------------
start(Supervisor_name,Registration_type,Start_info)->
    supervisor:start_link({Registration_type,Supervisor_name},?MODULE,Start_info).

%%--------------------------------------------------------------------
%% @doc
%%
%% Used to dynamically add a child to at run-time.
%%
%%
%% @end
%%--------------------------------------------------------------------
add_child(Supervisor_name,Child_name,Child_module,Child_type)->
        supervisor:start_child(Supervisor_name,generate_spec(Child_module,Child_name,Child_type)).


remove_child(Supervisor_name,Child_name)->
        supervisor:terminate_child(Supervisor_name,Child_name),
        supervisor:delete_child(Supervisor_name,Child_name).

%% Mandatory callback functions


%% Modify this function to do appropriate supervision initialization.
%%@private
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 10},
    ChildSpecs = [
        generate_spec(customer_request_server,crs1,worker),
        generate_spec(customer_request_server,crs2,worker),
        generate_spec(customer_request_server,crs3,worker),
        generate_spec(customer_request_server,crs4,worker),
        generate_spec(customer_request_server,crs5,worker),
        generate_spec(customer_request_server,crs6,worker),
        generate_spec(customer_request_server,crs7,worker),
        generate_spec(customer_request_server,crs8,worker),
        generate_spec(customer_request_server,crs9,worker),
        generate_spec(customer_request_server,crs10,worker),
        generate_spec(customer_request_server,crs11,worker),
        generate_spec(customer_request_server,crs12,worker),
        generate_spec(customer_request_server,crs13,worker),
        generate_spec(customer_request_server,crs14,worker),
        generate_spec(customer_request_server,crs15,worker),
        generate_spec(customer_request_server,crs16,worker),
        generate_spec(customer_request_server,crs17,worker),
        generate_spec(customer_request_server,crs18,worker),
        generate_spec(customer_request_server,crs19,worker),
        generate_spec(customer_request_server,crs20,worker),
        generate_spec(customer_request_server,crs21,worker),
        generate_spec(customer_request_server,crs22,worker),
        generate_spec(customer_request_server,crs23,worker),
        generate_spec(customer_request_server,crs24,worker),
        generate_spec(customer_request_server,crs25,worker),
        generate_spec(customer_request_server,crs26,worker),
        generate_spec(customer_request_server,crs27,worker),
        generate_spec(customer_request_server,crs28,worker),
        generate_spec(customer_request_server,crs29,worker),
        generate_spec(customer_request_server,crs30,worker),
        generate_spec(customer_request_server,crs31,worker),
        generate_spec(customer_request_server,crs32,worker),
        generate_spec(customer_request_server,crs33,worker),
        generate_spec(customer_request_server,crs34,worker),
        generate_spec(customer_request_server,crs35,worker),
        generate_spec(customer_request_server,crs36,worker),
        generate_spec(customer_request_server,crs37,worker),
        generate_spec(customer_request_server,crs38,worker),
        generate_spec(customer_request_server,crs39,worker),
        generate_spec(customer_request_server,crs40,worker),
        generate_spec(customer_request_server,crs41,worker),
        generate_spec(customer_request_server,crs42,worker),
        generate_spec(customer_request_server,crs43,worker),
        generate_spec(customer_request_server,crs44,worker),
        generate_spec(customer_request_server,crs45,worker),
        generate_spec(customer_request_server,crs46,worker),
        generate_spec(customer_request_server,crs47,worker),
        generate_spec(customer_request_server,crs48,worker),
        generate_spec(customer_request_server,crs49,worker),
        generate_spec(customer_request_server,crs50,worker),
        generate_spec(customer_request_server,crs51,worker),
        generate_spec(customer_request_server,crs52,worker),
        generate_spec(customer_request_server,crs53,worker),
        generate_spec(customer_request_server,crs54,worker),
        generate_spec(customer_request_server,crs55,worker),
        generate_spec(customer_request_server,crs56,worker),
        
        % generate_spec(customer_request_server,worker),
        generate_spec(update_price_server,worker)
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Modify this function to include an appropriate shutdown time, etc depending on which portions
%% of the child specification need to be set.
%%@private

generate_spec(Module,Type)->
%%
%% A child Specification is a record with the following mappings.
%%
%% child_spec() = #{id => child_id(),       % mandatory. The name to be registered.
%%                  start => mfargs(),      % mandatory. The module's startup function.
%%                  restart => atom(),              % optional. Options are permanent (restart always), transient (restart only after abnormal termination), and temporary (never restart).
%%                  shutdown => integer()|atom(),   % optional. A number or the atom infinity representing the milliseconds allowed for a soft, normal shutdown before it is killed brutally.
%%                  type => atom(),                 % optional. Options are worker or supervisor.
%%                  modules => [module()]|atom()}   % optional. A list of modules to be considered for upgrading
%%                                                  % when the child's code is upgraded. The dynamic atom is used for when 
%%                                                  % such a list is unknown, for example when the child is a 
%%                                                  % gen_event manager with some unknown types of gen_event handler
%%                                                  % modules to be added later.
        #{id => Module,
          start => {Module,start,[]},% This template forces local registration of the child and
                                                  % forces it to startup without parameters. 
          restart => permanent,
          shutdown => 2000,
          type => Type,
          modules => [Module]}.


generate_spec(Module,Name,Type)->
%%
%% A child Specification is a record with the following mappings.
%%
%% child_spec() = #{id => child_id(),       % mandatory. The name to be registered.
%%                  start => mfargs(),      % mandatory. The module's startup function.
%%                  restart => atom(),              % optional. Options are permanent (restart always), transient (restart only after abnormal termination), and temporary (never restart).
%%                  shutdown => integer()|atom(),   % optional. A number or the atom infinity representing the milliseconds allowed for a soft, normal shutdown before it is killed brutally.
%%                  type => atom(),                 % optional. Options are worker or supervisor.
%%                  modules => [module()]|atom()}   % optional. A list of modules to be considered for upgrading
%%                                                  % when the child's code is upgraded. The dynamic atom is used for when 
%%                                                  % such a list is unknown, for example when the child is a 
%%                                                  % gen_event manager with some unknown types of gen_event handler
%%                                                  % modules to be added later.
        #{id => Name,
          start => {Module,start,[local,Name,[]]},% This template forces local registration of the child and
                                                  % forces it to startup without parameters. 
          restart => permanent,
          shutdown => 2000,
          type => Type,
          modules => [Module]}.

-module(db_interface).
-export([get_prices/2,set_prices/2]).

%%% get_prices will send a request to the riak server to get the price of 
%%% each item in the list. add each of those items together
get_prices([H|_T],Riak_PID) ->
    % get price from riak
    % case riakc_pb_socket:get(Riak_PID, <<"store1">>, List) of 
	%     {ok,Fetched}->
	% 	%reply with the value as a binary, not the key nor the bucket.
	% 	{reply,binary_to_term(riakc_obj:get_value(Fetched)),Riak_PID};
	%      Error ->
	% 	{reply,Error,Riak_PID}
	% end.
	get_price(Riak_PID,<<"store1">>,H).

get_price(Riak_PID,Bucket,Key) ->
	riakc_pb_socket:get(Riak_PID, Bucket,Key).

set_prices(Map,Riak_PID) ->
	I = maps:iterator(Map,Riak_PID),
	set_price(Riak_PID,<<"store1">>,maps:next(I)).
    
set_price(Riak_PID,Bucket,{Key,Value}) ->
	riakc_obj:new(Bucket,Key,Value),	
	riakc_pb_socket:put(Riak_PID).
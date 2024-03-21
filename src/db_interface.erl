-module(db_interface).
-export([get_prices/2,set_prices/2]).

%%% get_prices will send a request to the riak server to get the price of 
%%% each item in the list. add each of those items together
get_prices(List,Riak_PID) ->
    % get price from riak
    % case riakc_pb_socket:get(Riak_PID, <<"store1">>, List) of 
	%     {ok,Fetched}->
	% 	%reply with the value as a binary, not the key nor the bucket.
	% 	{reply,binary_to_term(riakc_obj:get_value(Fetched)),Riak_PID};
	%      Error ->
	% 	{reply,Error,Riak_PID}
	% end.
	Stores = [<<"store1">>],
	Results = [get_location_prices(Riak_PID,lists:nth(1,Stores),List)],
	maps:from_list(lists:zip(Stores,Results)).

get_location_prices(Riak_PID,Bucket,List) ->
	New_list = lists:map(fun(X) -> 
		{ok, Fetched} = riakc_pb_socket:get(Riak_PID, Bucket,X), 
		Fetched end, 
		List),
	% {ok, Fetched} = riakc_pb_socket:get(Riak_PID, Bucket,Key),
	{_,Total} = lists:mapfoldl(fun(X,Acc) -> 
		Acc + binary_to_float(riakc_obj:get_value(X)) end, 0.0, New_list),
	Total.	

set_prices(Map,Riak_PID) ->
	Iterator = maps:iterator(Map,Riak_PID),
	set_price(Riak_PID,<<"store1">>,maps:next(Iterator)).

set_price(_,_,none) ->
	ok;
set_price(Riak_PID,Bucket,{Key,Value,Next_iterator}) ->
	Object = riakc_obj:new(Bucket,Key,Value),	
	riakc_pb_socket:put(Riak_PID, Object),
	set_price(Riak_PID,Bucket,maps:next(Next_iterator)).
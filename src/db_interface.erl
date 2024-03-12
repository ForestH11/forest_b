-module(db_interface).
-export([get_prices/1,set_prices/1]).

get_prices(_) ->
    % get price from riak
    {error,no_db}.

set_prices(_) ->
    {error,no_db}.
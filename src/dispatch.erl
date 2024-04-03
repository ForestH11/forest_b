-module(dispatch).
-export([customer_request/1]).

customer_request(Price_list) ->
    Name = rrobin_serv:next(),
    gen_server:call(Name, {prices_of, Price_list}).
-module(todo_SUITE).
-include_lib("eunit/include/eunit.hrl").
-export([all/0, get_all_test/1]).

all() -> [get_all_test].

get_all_test(_Config) ->
    %% Mock the database connection
    meck:new(todo_db, [passthrough]),
    meck:expect(todo_db, start_link, 0, {ok, fake_conn}),
    meck:expect(todo_db, get_todos, 1, {ok, [{1, <<"Buy groceries">>, false}]}),
    meck:expect(todo_db, stop_link, 1, ok),

    %% Create a mock request
    Req = #{method => <<"GET">>, bindings => #{}, body => <<>>, headers => #{<<"content-type">> => <<"application/json">>}, version => 'HTTP/1.1'},

    %% Call the function being tested
    State = [],
    {ok, Req2, _} = todos:get_all(Req, State),

    %% Check the return value
    ?assertEqual(200, cowboy_req:status(Req2)),
    ?assertEqual(<<"application/json">>, cowboy_req:header_value(<<"content-type">>, Req2)),

    %% Check the response body (JSON data)
    ExpectedBody = <<"[{\"id\":1,\"isChecked\":false,\"value\":\"Buy groceries\"}]">>,
    ?assertEqual(ExpectedBody, cowboy_req:body(Req2)),

    %% Unload the mock
    meck:unload(todo_db).








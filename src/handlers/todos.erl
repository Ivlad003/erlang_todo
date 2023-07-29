-module(todos).
-behaviour(cowboy_handler).

-export([init/2, get_all/2, create_todo/2, todo_update_check/2, delete_todo/2]).

%% @doc Initializes the request and routes it to the appropriate handler based on the HTTP method.
init(Req, State) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"GET">> ->
            get_all(Req, State);
        <<"POST">> ->
            create_todo(Req, State);
        <<"PUT">> ->
            todo_update_check(Req, State);
        <<"DELETE">> ->
            delete_todo(Req, State)
    end.

%% @doc Returns a list of all todos.
get_all(Req, State) ->
    try
        {ok, Conn} = todo_db:start_link(),
        {ok, Rows} = todo_db:get_todos(Conn),
        todo_db:stop_link(Conn),
        Maps = lists:map(
            fun({Id, Value, IsChecked}) ->
                #{<<"id">> => Id, <<"value">> => Value, <<"isChecked">> => IsChecked}
            end,
            Rows
        ),
        Body = jsx:encode(Maps),
        Headers = #{<<"content-type">> => <<"application/json">>},
        {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
        {ok, Req2, State}
    catch
        error:Reason:Stacktrace ->
            io:format("Error test1: ~p~nStacktrace: ~p~n", [Reason, Stacktrace]),
            ct:pal("Error test1: ~p~nStacktrace: ~p~n", [Reason, Stacktrace]),
            {halt, Req, State}
    end.



%% @doc Creates a new todo.
create_todo(Req, State) ->
    try
        {ok, Conn} = todo_db:start_link(),
        {ok, Body, Req1} = cowboy_req:read_body(Req),
        Todo = jsx:decode(Body, [return_maps]),
        Value = maps:get(<<"value">>, Todo),
        IsChecked = maps:get(<<"isChecked">>, Todo, false),
        ok = todo_db:add_todo(Conn, {Value, IsChecked}),
        todo_db:stop_link(Conn),
        {ok, Req2} = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req1),
        {ok, Req2, State}
    catch
        error:Reason ->
            io:format("Error: ~p~n", [Reason]),
            {halt, Req, State}
    end.

%% @doc Updates the checked status of a todo.
todo_update_check(Req, State) ->
    try
        {ok, Conn} = todo_db:start_link(),
        {ok, Body, Req1} = cowboy_req:read_body(Req),
        Todo = jsx:decode(Body, [return_maps]),
        Id = binary_to_integer(cowboy_req:binding(id, Req)),
        IsChecked = maps:get(<<"isChecked">>, Todo, false),
        ok = todo_db:update_todo_checked(Conn, Id, IsChecked),
        todo_db:stop_link(Conn),
        {ok, Req2} = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req1),
        {ok, Req2, State}
    catch
        error:Reason ->
            io:format("Error: ~p~n", [Reason]),
            {halt, Req, State}
    end.

%% @doc Deletes a todo.
delete_todo(Req, State) ->
    try
        {ok, Conn} = todo_db:start_link(),
        Id = binary_to_integer(cowboy_req:binding(id, Req)),
        ok = todo_db:delete_todo(Conn, Id),
        todo_db:stop_link(Conn),
        {ok, Req2} = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req),
        {ok, Req2, State}
    catch
        error:Reason ->
            io:format("Error: ~p~n", [Reason]),
            {halt, Req, State}
    end.

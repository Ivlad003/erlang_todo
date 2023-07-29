-module(todo_db).
-export([start_link/0, stop_link/1, add_todo/2, get_todos/1, delete_todo/2, update_todo_checked/3]).

start_link() ->
    try
        DBHost = application:get_env(erlang_todo, db_host, "localhost"),
        DBName = application:get_env(erlang_todo, db_name, "todo_db"),
        DBUser = application:get_env(erlang_todo, db_user, "postgres"),
        DBPassword = application:get_env(erlang_todo, db_password, "postgres"),
        % add all envs in io:format
        io:format("DBHost: ~p~n", [DBHost]),
        io:format("DBName: ~p~n", [DBName]),
        io:format("DBUser: ~p~n", [DBUser]),
        io:format("DBPassword: ~p~n", [DBPassword]),

        {ok, Conn} = epgsql:connect(DBHost, DBUser, DBPassword, [{database, DBName}]),
        {ok, Conn}
    catch
        error:Reason ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

stop_link(Conn) ->
    ok = epgsql:close(Conn).

add_todo(Conn, {Value, IsChecked}) ->
    {ok, _Count} = epgsql:equery(
        Conn, "INSERT INTO todos (value, ischecked) VALUES ($1, $2)", [
            Value, IsChecked
        ]
    ),
    ok.

get_todos(Conn) ->
    {ok, _, Rows} = epgsql:equery(Conn, "SELECT * FROM todos"),
    {ok, Rows}.

update_todo_checked(Conn, Id, IsChecked) ->
    {ok, _} = epgsql:equery(Conn, "UPDATE todos SET ischecked = $1 WHERE id = $2", [IsChecked, Id]),
    ok.
    

delete_todo(Conn, Id) ->
    {ok, _} = epgsql:equery(Conn, "DELETE FROM todos WHERE id = $1", [Id]),
    ok.

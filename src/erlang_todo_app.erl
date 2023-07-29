-module(erlang_todo_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = todo_router:routes(),
    {ok, _} = cowboy:start_clear(
        my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    erlang_todo_sup:start_link().

stop(_State) ->
    ok.

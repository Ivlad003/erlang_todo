-module(todo_router).
-export([routes/0]).

routes() ->
    cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, erlang_todo, "static/index.html"}},
            {"/todo", todos, []},
			{"/todo/:id", todos, []},
            {"/[...]",
                fun(Req, State) ->
                    {ok, Req2} = cowboy_req:reply(
                        405,
                        #{<<"content-type">> => <<"text/plain">>},
                        <<"Method Not Allowed">>,
                        Req
                    ),
                    {ok, Req2, State}
                end,
                []}
        ]}
    ]).

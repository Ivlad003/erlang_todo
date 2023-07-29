# Erlang Todo App

## Table of Contents

- [Introduction](#introduction)
- [Project Structure](#project-structure)
- [Running the Application](#running-the-application)
  - [Production](#production)
  - [Development](#development)

## Introduction

Erlang Todo App is a simple todo list application built with Erlang and Cowboy web server. It allows users to manage their tasks by adding new tasks and marking existing tasks as completed.

## Project Structure

```
├── Dockerfile ({Docker image configuration})
├── config
│   ├── sys.config ({Application system configuration})
│   └── vm.args ({Erlang VM configuration})
├── docker-compose.yml ({Docker Compose configuration for running the app})
├── init.sql ({SQL script for initializing the PostgreSQL database})
├── logs
│   └── ct.latest.log ({Logs of Common Test results})
├── postman.collection.json ({Postman collection of API requests for testing})
├── priv
│   └── static
│       └── index.html ({Static HTML file for web page})
├── readme.md ({Main documentation of the Erlang Todo App})
├── rebar.config ({Rebar3 build configuration})
├── rebar.lock ({Rebar3 dependencies lock file})
├── relx.config ({Release configuration for Erlang distribution})
├── src
│   ├── erlang_todo.app.src ({Application resource file})
│   ├── erlang_todo_app.erl ({Main application module})
│   ├── erlang_todo_sup.erl ({Supervision tree for application processes})
│   ├── handlers
│   │   └── todos.erl ({Handler module for managing todos})
│   ├── todo_db.erl ({Data storage and database handler})
│   └── todo_router.erl ({Router for handling API routes})
└── test
    └── todo_SUITE.erl ({Common Test suite for testing the application})
```

## Running the Application

### Production

To run the application in production mode, follow these steps:

1. Build the release:

   ```
   rebar3 release
   ```

2. Start the application:

   ```
   _build/default/rel/erlang_todo_release/bin/erlang_todo_release daemon
   ```

3. Stop the application:

   ```
   _build/default/rel/erlang_todo_release/bin/erlang_todo_release stop
   ```

### Development

To run the application in development mode, follow these steps:

1. Compile the code:

   ```
   rebar3 compile
   ```

2. Start the application shell:

   ```
   rebar3 shell
   ```

## Docker Configuration

You can also run the application using Docker with a PostgreSQL database. Use the following Docker command to set up the database:

```
docker run --name db -e POSTGRES_DB=todo_db -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=postgres -e POSTGRES_PORT=5432 -v $(pwd)/init.sql:/docker-entrypoint-initdb.d/init.sql -p 5432:5432 -d postgres:13
```

**Note**: Replace `$(pwd)/init.sql` with the path to your SQL script for initializing the database.

---
Feel free to update this README with more specific details about your application as needed. Happy coding!
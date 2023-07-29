# Erlang Todo App

## Table of Contents

- [Introduction](#introduction)
- [Project Structure](#project-structure)
- [Running the Application](#running-the-application)
  - [Production](#production)
  - [Development](#development)
- [Erlang Libraries Used](#erlang-libraries-used)
- [Include Libraries in index.html](#include-libraries-in-indexhtml)

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

## Erlang Libraries Used

The Erlang Todo App utilizes the following libraries:

- [Cowboy](https://github.com/ninenines/cowboy): A small, fast, and flexible HTTP server for Erlang.
- [epgsql](https://github.com/epgsql/epgsql): A PostgreSQL database driver for Erlang.
- [jsx](https://github.com/talentdeficit/jsx): A JSON encoder and decoder for Erlang.

## JavaScript Libraries:
- React: [https://unpkg.com/react@17/umd/react.production.min.js](https://unpkg.com/react@17/umd/react.production.min.js)
- ReactDOM: [https://unpkg.com/react-dom@17/umd/react-dom.production.min.js](https://unpkg.com/react-dom@17/umd/react-dom.production.min.js)
- Babel Standalone: [https://unpkg.com/babel-standalone@6/babel.min.js](https://unpkg.com/babel-standalone@6/babel.min.js)
- Axios: [https://unpkg.com/axios/dist/axios.min.js](https://unpkg.com/axios/dist/axios.min.js)

## CSS Library:
- Bootstrap: [https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css](https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css)

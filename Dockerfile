# Use the official Erlang/OTP base image from Docker Hub
FROM erlang:24

# Set the working directory inside the container
WORKDIR /app

# Copy the Erlang project's source code and rebar.config file into the container
COPY src/ src/
COPY rebar.config .

# Compile the Erlang project using Rebar3
RUN rebar3 compile

# Expose the port that Cowboy will listen on (change this to match your application's port)
EXPOSE 8080

# Run the Erlang application using the Erlang shell
CMD ["erl", "-pa", "_build/default/lib/*/ebin", "-eval", "application:ensure_all_started(erlang_todo)."]

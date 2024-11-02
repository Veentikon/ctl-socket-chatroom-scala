% CMPT 436
% Assignment 3
% Vitalii Radzividlo
% vir138


-module(client).
-export([start/0]).

% start the client application
start() -> 
    io:format("Commands available:
    create <RoomName> - create new room
    join <RoomName> - join existing room
    leave <RoomName> - leave joined room
    send <RoomName> <Message> - send Message to RoomName~n"),
    register(client, spawn(fun() -> client() end)).

client() ->
    {ok, Socket} = gen_tcp:connect("server", 8080, [{active, false}, binary, {packet, 2}]),  
    % packet 2 specifies that each message will be preceded by two bytes, which will contain the length of the message
    io:format("Connecting to server ...\n"),
    spawn(fun() -> listen_server(Socket, self()) end),
    chat(Socket).

chat(Socket) ->
    io:format(""),
    Input = io:get_line(""),
    CleanedInput = string:strip(Input, both, $\n),
    {Command, Args} = parse_input(CleanedInput),

    Cont = process_command(Command, Args, Socket),               % terminate if receive 1
    if 
        Cont == 1 -> chat(Socket);
        true -> ok = gen_tcp:close(Socket), ok
    end.


% listen for incoming messages from the server
listen_server(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->

            io:format(Data),
            io:format("~n"),

            listen_server(Socket, Pid);
        {error, closed} ->
            io:format("Error: Socket closed, exiting process ...\n"),
            exit(closed);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            listen_server(Socket, Pid)
    end.


parse_input(Input) ->
    [Command | Args] = string:tokens(Input, " "),
    {Command, string:join(Args, " ")}.


process_command("help", Args, Socket) ->
    io:format("Commands: help, rooms, create <name>, quit, join <name>, leave\n"),
    1;

process_command("rooms", Args, Socket) ->
    gen_tcp:send(Socket, "rooms"),
    1;

process_command("create", Args, Socket) ->
    gen_tcp:send(Socket, "create " ++ Args),
    1;

process_command("quit", Args, Socket) ->
    gen_tcp:send(Socket, "quit"),
    0;

process_command("join", Args, Socket) ->
    gen_tcp:send(Socket, "join " ++ Args),
    1;

process_command("leave", Args, Socket) ->
    gen_tcp:send(Socket, "leave " ++ Args),
    1;

process_command("send", Args, Socket) ->
    gen_tcp:send(Socket, "send " ++ Args),
    1;

process_command(Command, Args, Socket) ->
    io:format("Unknown command: ~s, Arguments: ~p~n\n", [Command, Args]),
    1.


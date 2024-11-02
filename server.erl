% CMPT 436
% Assignment 3
% Vitalii Radzividlo
% vir138


-module(server).
-export([start/0]).

% Structure (record) that represents a chat room
-record(chat_room, {
    name :: binary(),
    subscribers :: [pid()],   % Pids of processes to who will receive updates on changes to the chat room
    messages :: [binary()]
}).

% ----------------------------- Initialize/start Server --------------------------------
start() ->                                                                  % start of the server application, this function is "exposed"
    register(server, spawn(fun() -> init_server() end)),                    % spawn client connection listener process
    register(notifier, spawn(fun() -> rooms_manager([], []) end)).          % spawn notification subscription process

init_server() ->
    {ok, ListenSocket} = gen_tcp:listen(8080, [{active, false}, binary, {packet, 2}]),
    io:format("Server is listening on port 8080~n"),
    server(ListenSocket, []).
% --------------------------------------------------------------------------------------

% Server loop process that listens for incoming connections
server(ListenSocket, State) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->

            % Spawn client request listener and a client loop (request processing)
            ClientLoop = spawn(fun() -> client_loop(State, Socket) end),
            spawn(fun() -> receive_client_data(Socket, ClientLoop) end),    % process that receives messages from client over tcp

            notifier ! {subscribe, ClientLoop},                                            % subscribe the ClientLoop process to notification_server

            server(ListenSocket, State);
        {error, Reason} ->
            io:format("Error accepting socket: ~p~n", [Reason]),
            server(ListenSocket, State)
    end.


% Receive messages/commands from the client over TCP, process them, and send them to the command handler
receive_client_data(Socket, ClientHandler) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            CharList = binary_to_list(Data),
            {Command, Rest} = get_one(CharList),

            % Format and redirect requests for command processing
            case Command of
                "quit" ->
                    ClientHandler ! {quit};
                "create" ->
                    ClientHandler ! {create, Rest};
                "join" ->
                    ClientHandler ! {join, Rest};
                "leave" ->
                    ClientHandler ! {leave, Rest};
                "rooms" ->
                    ClientHandler ! {rooms};
                "send" ->
                    {RoomName, Message} = get_one(Rest),
                    ClientHandler ! {newMsg, RoomName, Message};
                _ ->
                    io:format("Unknown command: ~p~n", [Command])
            end,

            % Continue receiving
            receive_client_data(Socket, ClientHandler);

        % Error handling
        {error, closed} ->
            io:format("Client connection closed~n"),
            ok;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            ok
    end.



% Process client requests as received from listener
client_loop(State, Socket) ->                           % use state to determine the chat loop to use, and control flow
    receive                                             % delegate tasks to other functions/processes
        {quit} ->
            io:format("closing socket ... ~n"),
            gen_tcp:close(Socket);
        {create, Rest} ->
            notifier ! {new_room, Rest, Socket},
            client_loop(State, Socket);
        {join, Rest} ->
            notifier ! {join_room, self(), Rest, Socket},
            client_loop(State, Socket);
        {leave, Rest} ->
            notifier ! {leave_room, self(), Rest},
            client_loop(State, Socket);
        {rooms} ->
            notifier ! {get_rooms, Socket},
            client_loop(State, Socket);
        {send_msg, RoomName, MSG} ->          % Handle new messages from the client
            try                               % test whether the connection is lost
                gen_tcp:send(Socket, RoomName ++ ": " ++ MSG),  
                client_loop(State, Socket)
            catch
                {error, Reason} ->
                    exit(Reason)
            end;
        {newMsg, Name, Msg} ->    % New message from other client/process
            notifier ! {newMsg, self(), Msg, Name},   % rooms manager can compare this Pid and skip it in the broadcast
            client_loop(State, Socket)
    end.


% ---------------------- process for managing shared resource ------------------------
% server which is used to communicate updates to the chat rooms to the subscribed processes via message passing
rooms_manager(Subscribers, ChatRooms) ->
    receive
        {subscribe, Pid} ->
            rooms_manager([Pid | Subscribers], ChatRooms);
        {unsubscribe, Pid} ->
            NewSubscribers = lists:delete(Pid, Subscribers),
            rooms_manager(NewSubscribers, ChatRooms);
        {new_room, Name, Socket} ->
            case create_room(Name, ChatRooms) of
                {error, _} ->
                    gen_tcp:send(Socket, "Room with this name already exists"),
                    rooms_manager(Subscribers, ChatRooms);
                {ok, NewChatRooms} ->
                    gen_tcp:send(Socket, "Room created"),
                    rooms_manager(Subscribers, NewChatRooms)
            end;
        {get_rooms, Socket} ->
            rooms_getter(Socket, ChatRooms),
            rooms_manager(Subscribers, ChatRooms);
        {leave_room, Pid, Name} ->
            NewChatRooms = leave_room(Pid, Name, ChatRooms),
            rooms_manager(Subscribers, NewChatRooms);
        {join_room, Pid, Name, Socket} ->
            case join_room(Pid, Name, ChatRooms) of 
                NewChatRooms ->
                    gen_tcp:send(Socket, "Joined"),
                    rooms_manager(Subscribers, NewChatRooms);
                ok ->
                    gen_tcp:send(Socket, "Room with this name does not exist"),
                    rooms_manager(Subscribers, ChatRooms)
            end;
        {newMsg, Pid, Msg, Name} ->
            NewChatRooms = add_msg(Pid, Msg, Name, ChatRooms),
            rooms_manager(Subscribers, NewChatRooms);
        _ ->
            rooms_manager(Subscribers, ChatRooms)
    end.


% retrieve a word from a string
get_one(Input) ->
    [Command | Args] = string:tokens(Input, " "),
    {Command, string:join(Args, " ")}.
% ----------------------------------------------------------------------------------------------



% ------------------------------------ Client Request Handling functions --------------------------------
% Retreive and send all room names to the client via tcp Socket
rooms_getter(Socket, ChatRooms) ->
    io:format("Getting rooms~n"),                                     % ----------------- status/log
    % get room names
    RoomNames = [Room#chat_room.name || Room <- ChatRooms],
    % join the room names into a single string with a comma delimiter
    RoomNamesString = string:join(RoomNames, ", "),
    % convert the string to binary 
    RoomNamesBinary = list_to_binary(RoomNamesString),
    gen_tcp:send(Socket, RoomNamesBinary).


create_room(RoomName, ChatRooms) ->
    case find_room(RoomName, ChatRooms) of
        {value, _} ->
            {error, exists};
        {error, not_found} ->
            io:format("Creating room: ~p~n", [RoomName]),           % ----------------- status/log
            NewRoom = #chat_room{name = RoomName, subscribers = [], messages = []},
            NewChatRooms = [NewRoom | ChatRooms],
            {ok, NewChatRooms}
     end.


join_room(PID, RoomName, ChatRooms) ->
    case find_room(RoomName, ChatRooms) of
        {value, Room} ->
            io:format("Joining room: ~p~n", [RoomName]),
            NewRoom = Room#chat_room{subscribers = [PID | Room#chat_room.subscribers]},

            % Send all existing messages to the new subscriber
            lists:foreach(fun(Message) -> PID ! {send_msg, RoomName, Message} end, Room#chat_room.messages),
            
            % Update the list of ChatRooms
            NewChatRooms = lists:map(
                fun (R) when R#chat_room.name == RoomName -> NewRoom;
                    (R) -> R
                end,
                ChatRooms
            ),
            NewChatRooms;
        {error, Reason} ->
            io:format("Error: ~p~n\n", [Reason]),
            ok
    end.


% Remove PID from the list of subscribers in the given chat room
leave_room(PID, RoomName, ChatRooms) ->
    case find_room(RoomName, ChatRooms) of
        {value, Room} ->
            io:format("Leaving room: ~p~n", [RoomName]),

            NewSubscribers = lists:delete(PID, Room#chat_room.subscribers),
            NewRoom = Room#chat_room{subscribers = NewSubscribers},

            % Update the list of ChatRooms
            NewChatRooms = lists:map(
                fun (R) when R#chat_room.name == RoomName -> NewRoom;
                    (R) -> R
                end,
                ChatRooms
            ),

            NewChatRooms;
        {error, Reason} ->
            io:format("Error: ~p~n\n", [Reason]),
            ok
    end.


%% Function to search for a room by name
find_room(RoomName, ChatRooms) ->
    lists:foldl(
        fun(Room, Acc) ->
            case Room#chat_room.name of
                RoomName ->
                    {value, Room};
                _ ->
                    Acc
            end
        end, {error, not_found}, ChatRooms).


add_msg(Pid, Msg, RoomName, ChatRooms) ->
    case find_room(RoomName, ChatRooms) of
        {value, Room} ->
            io:format("Adding new message to room: ~p~n", [RoomName]),
            
            % First, send the message to all subscribed processes
            SubscribersToNotify = lists:filter(fun(Subscriber) -> Subscriber /= Pid end, Room#chat_room.subscribers), % filter out the Pid of sender
            lists:foreach(fun(PID) -> PID ! {send_msg, RoomName, Msg} end, SubscribersToNotify),

            % Update the messages
            NewMessages = lists:append(Room#chat_room.messages, [Msg]),  % Append the new message to the list of messages
            NewRoom = Room#chat_room{messages = NewMessages},  % Update the room
            NewChatRooms = lists:map(
                fun (R) when R#chat_room.name == RoomName -> NewRoom;
                    (R) -> R
                end,
                ChatRooms
            ),
            NewChatRooms;

        {error, Reason} -> 
            io:format("Error: ~p~n\n", [Reason]),
            ChatRooms
    end.




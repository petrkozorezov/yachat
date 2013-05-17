-module(connection).

-export([start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ranch protocol callback
start_link(Ref, Sock, Transport, Opts) ->
    gen_server:start_link(?MODULE, {Ref, Sock, Transport, Opts}, []).

%% gen_server callbacks
-record(state, {
    handler_state        :: term(),
    transport            :: module(),
    sock                 :: port(),
    ref
}).

init({Ref, Sock, Transport, _Opts}) ->
    process_flag(trap_exit, true),
    S = #state{transport=Transport, sock=Sock, ref=Ref},
    {ok, S, 0}.

handle_call(Call, {_, From}, S) ->
    lager:error("unknown call ~p from ~p", [Call, From]),
    {reply, ok, S, hibernate}.

handle_cast(Cast, S) ->
    lager:error("unknown cast ~p", [Cast]),
    {noreply, S, hibernate}.

handle_info(timeout, S0=#state{ref=Ref}) ->
    lager:md(orddict:store(ip, ip(S0), lager:md())),
    lager:info("server: client connected"),
    ok = ranch:accept_ack(Ref),
    S1 = process_handler_result(handle_connect(), S0),
    active_once(S0),
    {noreply, S1#state{ref=undefined}, hibernate};
handle_info({tcp, Sock, Packet}, S=#state{sock=Sock}) ->
    NS = process_handler_result(handle_packet(Packet, S#state.handler_state), S),
    active_once(NS),
    {noreply, NS, hibernate};
handle_info({tcp_closed, Sock}, S=#state{sock=Sock}) ->
    {stop, normal, S};
handle_info({tcp_error, Sock, _Reason}, S=#state{sock=Sock}) ->
    {stop, normal, S};
handle_info({yachat, Info}, S) ->
    NS = process_handler_result(handle_yachat_info(Info, S#state.handler_state), S),
    {noreply, NS, hibernate};
handle_info(Info, S) ->
    lager:error("unknown info ~p", [Info]),
    {noreply, S, hibernate}.

terminate(_Reason, S) ->
    lager:info("server: client disconnected"),
    handle_disconnect(S#state.handler_state).

code_change(_OldVsn, S, _Extra) ->
    {ok, S, hibernate}.

%% local
process_handler_result({reply, Reply, NewHandlerState}, S) ->
    send(Reply, S),
    S#state{handler_state=NewHandlerState};
process_handler_result({noreply, NewHandlerState}, S) ->
    S#state{handler_state=NewHandlerState};
process_handler_result(stop, S) ->
    throw({stop, normal, S});
process_handler_result(HandlerResult, S) ->
    error(badard, [HandlerResult, S]).

send(NotifiesList, S) when is_list(NotifiesList) ->
    [send(Notify, S) || Notify <- NotifiesList];
send(Notify, #state{transport=Transport, sock=Sock}) ->
    lager:info("server: -> ~p", [Notify]),
    Packet = iolist_to_binary(protocol:write(s2c, Notify)),
    Transport:send(Sock, Packet).

active_once(#state{transport=Transport, sock=Sock}) ->
    Transport:setopts(Sock, [{active, once}, {packet, line}]).

%% handlers
handle_connect() ->
    Greeting = [
                "Hello from Yachat server! (^_^)",
                "",
                "You can use following commands:",
                "  enter <name>                enter with name",
                "  msg <msg>                   send broadcast message",
                "  privmsg <user_name> <msg>   send private message",
                "  leave                       leave chat",
                "  users                       get online users list",
                "",
                "Name must contain only letters and digits.",
                "",
                "Please enter first."
    ],
    {reply, [{sysmsg_notify, Line} || Line <- Greeting], undefined}.

handle_packet(Packet, S) ->
    try
        Cmd = protocol:read(c2s, Packet),
        lager:info("server: <- ~p", [Cmd]),
        handle_command(Cmd, S)
    catch 
        throw:E ->
            lager:warning("server: error ~p while handling packet ~p~n ** State: ~p~n ** Stacktrace: ~p",
                [E, Packet, S ,erlang:get_stacktrace()]),
            {reply, {error_notify, format_error(E)}, S};
        _T:E ->
            lager:error("server: unexpected error ~p while handling packet ~p~n ** State: ~p~n ** Stacktrace: ~p",
                [E, Packet, S ,erlang:get_stacktrace()]),
            {reply, {error_notify, "internal_error"}, S}
    end.

handle_command({enter_cmd, Name}, undefined) ->
    yachat:enter(Name),
    {noreply, Name};
handle_command(leave_cmd, _) ->
    stop;
handle_command(users_cmd, Name) ->
    Msg = ["online users:" | yachat:list_users()],
    {reply, [{sysmsg_notify, Line} || Line <- Msg], Name};
handle_command(_, S=undefined) ->
    {reply, {error_notify, "not_logged_in"}, S};
handle_command({privmsg_cmd, To, Msg}, Name) ->
    yachat:private_message(To, Name, Msg),
    {noreply, Name};
handle_command({msg_cmd, Msg}, Name) ->
    yachat:message(Name, Msg),
    {noreply, Name};
handle_command(_Cmd, Name) ->
    {reply, {error_notify, "invalid_command"}, Name}.

handle_yachat_info({private_message, From, Msg}, Name) ->
    {reply, {privmsg_notify, From, Msg}, Name};
handle_yachat_info({broadcast_message, From, Msg}, Name) ->
    {reply, {msg_notify, From, Msg}, Name};
handle_yachat_info({system_message, Msg}, Name) ->
    {reply, {sysmsg_notify, Msg}, Name};
handle_yachat_info(Info, Name) ->
    lager:log(error, "unknown yachat info ~p", [Info]),
    {noreply, Name}.

handle_disconnect(undefined) ->
    ok;
handle_disconnect(Name) ->
    yachat:leave(Name).


%% utils
format_error(E) ->
    io_lib:format("~p", [E]).

ip(#state{transport=Transport, sock=Sock}) ->
    {ok, {Addr, Port}} = Transport:peername(Sock),
    lists:flatten(io_lib:format("~s:~p", [inet_parse:ntoa(Addr), Port])).

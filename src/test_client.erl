-module(test_client).

-export([connect/3]).
-export([enter  /2]).
-export([leave  /1]).
-export([msg    /2]).
-export([privmsg/3]).
-export([users  /1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


connect(Host, Port, NotifyHandler) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {Host, Port, NotifyHandler}, []),
    Pid.

enter(Pid, Name) ->
    cmd(Pid, {enter_cmd, Name}).

leave(Pid) ->
    cmd(Pid, leave_cmd).

msg(Pid, Msg) ->
    cmd(Pid, {msg_cmd, Msg}).

privmsg(Pid, User, Msg) ->
    cmd(Pid, {privmsg_cmd, User, Msg}).

users(Pid) ->
    cmd(Pid, users_cmd).

cmd(Pid, Cmd) ->
    gen_server:call(Pid, {cmd, Cmd}).


%% gen_server API
-record(state, {
    sock,
    notify_handler
}).


init({Host, Port, NotifyHandler}) ->
    lager:info("test client: connecting ~s:~p", [Host, Port]),
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, line}, {active, true}]),
    S = #state{
        sock=Sock,
        notify_handler=NotifyHandler
    },
    {ok, S}.

handle_call({cmd, Cmd}, _From, S) ->
    send(Cmd, S),
    {reply, ok, S};
handle_call(Call, _From, S) ->
    exit({'unknown call', Call}),
    {reply, ok, S}.

handle_cast(Cast, S) ->
    exit({'unknown cast', Cast}),
    {noreply, S}.

handle_info({tcp, Sock, Data}, S=#state{sock=Sock}) ->
    handle_notify(Data, S),
    {noreply, S};
handle_info({tcp_closed, Sock}, S=#state{sock=Sock}) ->
    {stop, normal, S};
handle_info({tcp_error, Sock, _Reason}, S=#state{sock=Sock}) ->
    {stop, normal, S};
handle_info(Info, S) ->
    exit({'unknown info', Info}),
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


%% local
handle_notify(Packet, #state{notify_handler=NotifyHandler}) ->
    Notify = protocol:read(s2c, Packet),
    lager:info("test client: <- ~p", [Notify]),
    NotifyHandler(Notify).

send(Cmd, #state{sock=Sock}) ->
    lager:info("test client: -> ~p", [Cmd]),
    gen_tcp:send(Sock, protocol:write(c2s, Cmd)).

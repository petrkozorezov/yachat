-module(yachat_tests).
-include_lib("eunit/include/eunit.hrl").

-define(HOST, "localhost").
-define(PORT, 8081).

do_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun(_) ->
            [
                ?_assertEqual(run(5), ok)
                % ?_assertEqual(protocol_parser_test(), ok)
            ]
        end
    }.

start() ->
    application:load(yachat),
    Config = [
        {port, ?PORT}
    ],
    [application:set_env(yachat, K, V) || {K, V} <- Config],

    Apps = utils:test_application_start(yachat),

    dbg:tracer(), dbg:p(all,c),
    % dbg:tpl(test_client, x),

    Apps.

stop(Apps) ->
    %% ranch doesn't wait for his children
    timer:sleep(1000),
    utils:test_application_stop(Apps).


run(N) ->
    Self = self(),
    NotifyHandler = fun(Event) -> send(Self, Event) end,

    Users = [{"user"++integer_to_list(I), test_client:connect(?HOST, ?PORT, NotifyHandler)} || I <- lists:seq(1, N)],
    [flush(P) || {_, P} <- Users],
    enter_all(Users),

    test_broadcast(Users),
    test_private(Users),

    leave_all(Users),

    ok.

enter_all(Users) ->
    enter_all(Users, []).
enter_all([], _) ->
    ok;
enter_all([User|RemainObservers], EnteredObservers) ->
    enter(User, EnteredObservers),
    enter_all(RemainObservers, [User|EnteredObservers]).

enter({Name, Pid}, Observers) ->
    test_client:enter(Pid, Name),
    {sysmsg_notify, _} = recv(Pid),
    % flush(Pid),
    lists:foreach(fun(P) -> {sysmsg_notify, _} = recv(P) end, [P || {_, P} <- Observers]).

test_broadcast(Users=[{FromName, FromP}|_]) ->
    Msg = "hello",
    test_client:msg(FromP, Msg),
    lists:foreach(
        fun({_, Pid}) ->
            {msg_notify, FromName, Msg} = recv(Pid)
        end, Users).

test_private([{FromName, FromP}, {ToName, ToP} | Others]) ->
    Msg = "hello",
    test_client:privmsg(FromP, ToName, Msg),
    {privmsg_notify, FromName, Msg} = recv(FromP),
    {privmsg_notify, FromName, Msg} = recv(ToP),
    lists:foreach(
        fun({_, Pid}) ->
            timeout = recv(Pid, 10)
        end, Others).

leave_all([]) ->
    ok;
leave_all([User|RemainObservers]) ->
    leave(User, RemainObservers),
    leave_all(RemainObservers).

leave({_, Pid}, Observers) ->
    OldTrap = process_flag(trap_exit, true),
    test_client:leave(Pid),
    receive
        {'EXIT', Pid, normal} -> ok
    after
        5000 -> exit(test_client_stop_timeout)
    end,
    lists:foreach(fun(P) -> {sysmsg_notify, _} = recv(P) end, [P || {_, P} <- Observers]),
    process_flag(trap_exit, OldTrap).

%% local
recv(Pid) ->
    recv(Pid, 1000).
recv(Pid, Timeout) ->
    receive 
        {msg, Pid, Msg} ->
            Msg
    after Timeout ->
        timeout
    end.
send(Self, Msg) ->
    Self ! {msg, self(), Msg}.

flush(Pid) ->
    case recv(Pid, 10) of
        timeout -> ok;
        _       -> flush(Pid)
    end.

-module(protocol).

-export([read/2]).
-export([write/2]).

%% API
read(Direction, Bin) when is_binary(Bin) ->
    read(Direction, binary_to_list(Bin));
read(c2s, Str) when is_list(Str) ->
    parse_cmd(parse_cmd_name(skip_spaces(cut_rn(Str))));
read(s2c, Str) when is_list(Str) ->
    parse_notify(cut_rn(Str)).

write(c2s, Cmd) ->
    [write_cmd(Cmd), "\r\n"];
write(s2c, Notify) ->
    [write_notify(Notify), "\r\n"].

%% local
%% commands reading
parse_cmd_name("enter"   ++ T) -> {enter_cmd  , T};
parse_cmd_name("leave"   ++ T) -> {leave_cmd  , T};
parse_cmd_name("privmsg" ++ T) -> {privmsg_cmd, T};
parse_cmd_name("msg"     ++ T) -> {msg_cmd    , T};
parse_cmd_name("users"   ++ T) -> {users_cmd  , T};
parse_cmd_name(_             ) -> throw(invalid_cmd_name).

parse_cmd({Cmd, Str}) ->
    parse_cmd(Cmd, read_spaces(Str)).
parse_cmd(Cmd=enter_cmd, Str) ->
    {Name, Str1} = read_name(Str),
    [] = skip_spaces(Str1),
    {Cmd, Name};
parse_cmd(Cmd=leave_cmd, []) ->
    Cmd;
parse_cmd(Cmd=privmsg_cmd, Str0) ->
    {Name, Str1} = read_name(Str0),
    {Cmd, Name, read_msg(read_spaces(Str1))};
parse_cmd(Cmd=msg_cmd, Str) ->
    {Cmd, read_msg(Str)};
parse_cmd(Cmd=users_cmd, []) ->
    Cmd.

%% commands writing
write_cmd({enter_cmd, Name}       ) -> ["enter ", Name];
write_cmd( leave_cmd              ) ->  "leave";
write_cmd({privmsg_cmd, Name, Msg}) -> ["privmsg ", Name, " ", Msg];
write_cmd({msg_cmd, Msg}          ) -> ["msg ", Msg];
write_cmd( users_cmd              ) ->  "users".

%% notifies readinf
parse_notify("error: "++T) ->
    {error_notify, T};
parse_notify("["++T) ->
    {Name, T1} = read_name(T),
    "]: " ++ Msg = T1,
    {msg_notify, Name, Msg};
parse_notify("<"++T) ->
    {Name, T1} = read_name(T),
    ">: " ++ Msg = T1,
    {privmsg_notify, Name, Msg};
parse_notify("> "++T) ->
    {sysmsg_notify, T};
parse_notify(_) ->
    throw(invalid_notify).

%% notifies writing
write_notify({error_notify, Err}) ->
    io_lib:format("error: ~s", [Err]);
write_notify({msg_notify, Name, Msg}) ->
    io_lib:format("[~s]: ~s", [Name, Msg]);
write_notify({privmsg_notify, Name, Msg}) ->
    io_lib:format("<~s>: ~s", [Name, Msg]);
write_notify({sysmsg_notify, Msg}) ->
    io_lib:format("> ~s", [Msg]).

%% utils
-define(IS_DIGIT(C), (C >= $0 andalso C =< $9)).
-define(IS_UPPER(C), (C >= $A andalso C =< $Z)).
-define(IS_LOWER(C), (C >= $a andalso C =< $z)).
-define(IS_LETTER(C), (?IS_UPPER(C) orelse ?IS_LOWER(C))).
-define(IS_SPACE(C), (C =:= $\s orelse C =:= $\t)).

read_name(Str) ->
    read_name(Str, []).
read_name([], []) ->
    throw(empty_name);
read_name([], Acc) ->
    {lists:reverse(Acc), []};
read_name([H|T], Acc) when ?IS_DIGIT(H) orelse ?IS_LETTER(H) ->
    read_name(T, [H|Acc]);
read_name(Str, Acc) ->
    {lists:reverse(Acc), Str}.

read_msg([]) ->
    throw(empty_message);
read_msg(Msg) ->
    Msg.

skip_spaces([H|T]) when ?IS_SPACE(H) -> skip_spaces(T);
skip_spaces(Str) -> Str.

read_spaces([]) -> [];
read_spaces([H|T]) when ?IS_SPACE(H) -> skip_spaces(T);
read_spaces([H|_]) -> throw({syntax_error, [H]}).

cut_rn(Str) ->
    {Str1, "\r\n"} = lists:split(length(Str)-2, Str),
    Str1.

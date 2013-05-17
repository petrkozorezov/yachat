-module(yachat).
-behaviour(application).
-behaviour(supervisor).

%% API
-export([start/0]).
-export([stop/0]).
-export([enter/1]).
-export([leave/1]).
-export([private_message/3]).
-export([system_message/1]).
-export([message/2]).
-export([list_users/0]).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([init/1]).


%% API
start() ->
    utils:start_application(?MODULE).

stop() ->
    application:stop(?MODULE).


enter(Name) ->
    case gproc:lookup_pids({p, l, {user, Name}}) of
        [] -> ok;
        _  -> throw(already_registered)
    end,
    gproc:reg(reg_tuple(world)),
    gproc:reg(reg_tuple({user, Name})),
    system_message([Name, " entered to the chat"]).

leave(Name) ->
    gproc:unreg(reg_tuple(world)),
    gproc:unreg(reg_tuple({user, Name})),
    system_message([Name, " leaved the chat"]).

private_message(To, From, Msg) ->
    send_yachat_info({user, To  }, {private_message, From, Msg}),
    send_yachat_info({user, From}, {private_message, From, Msg}).

system_message(Msg) ->
    send_yachat_info(world, {system_message, Msg}).

message(From, Msg) ->
    send_yachat_info(world, {broadcast_message, From, Msg}).

list_users() ->
    %% qlc select across all online users
    gproc:select({all,all},[{{{'_', '_', {user, '$1'}}, '_', '_'}, [], ['$1']}]).


send_yachat_info(Channel, Info) ->
    gproc:send(reg_tuple(Channel), {yachat, Info}).

reg_tuple(Name) ->
    {p, l, Name}.

%% application callbacks
start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    {ok, _} = ranch:start_listener(yachat, 1, ranch_tcp, [{port, Port}], connection, []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ranch:start_listener(yachat).


%% supervisor callbacks
-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(news_bot)]} }.

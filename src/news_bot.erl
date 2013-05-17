-module(news_bot).
-include_lib("xmerl/include/xmerl.hrl").

-export([start_link/0, fetch_latest/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link(?MODULE, [], []).

-define(NAME, "news").
-record(state, {}).

init([]) ->
    process_flag(trap_exit, true),
    yachat:enter(?NAME),
    {ok, #state{}, hibernate}.

handle_call(Call, _From, S) ->
    exit({'unknown call', Call}),
    {reply, ok, S, hibernate}.

handle_cast(Cast, S) ->
    exit({'unknown cast', Cast}),
    {noreply, S, hibernate}.

handle_info({yachat, Info}, S) ->
    handle_yachat_info(Info),
    {noreply, S, hibernate};
handle_info(Info, S) ->
    exit({'unknown info', Info}),
    {noreply, S, hibernate}.

terminate(_Reason, _S) ->
    yachat:leave(?NAME),
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S, hibernate}.


%% local
handle_yachat_info({private_message, ?NAME, _}) ->
    ok;
handle_yachat_info({private_message, From, "hello"}) ->
    yachat:private_message(From, ?NAME, "привет");
handle_yachat_info({private_message, From, "latest"}) ->
    [yachat:private_message(From, ?NAME, Line) || Line <- make_latest_msg(fetch_latest())];
handle_yachat_info({private_message, From, _}) ->
    yachat:private_message(From, ?NAME, "unknown command");
handle_yachat_info({broadcast_message, _From, _Msg}) ->
    ok;
handle_yachat_info({system_message, _Msg}) ->
    ok;
handle_yachat_info(Info) ->
    lager:log(error, "unknown yachat info ~p", [Info]).


%% fetching last news from news.yandex.ru RSS channel
make_latest_msg({ok, Latest}) ->
    [
        "top latest news:" |
        lists:map(
            fun(E) ->
                binary_to_list(unicode:characters_to_binary(E))
            end,
        Latest)
    ];
make_latest_msg(error) ->
    "error while fetching news".

fetch_latest() ->
    try
        {ok, "2" ++ _, _, Body} = ibrowse:send_req("http://news.yandex.ru/index.rss", [{"Accept-Encoding", "gzip"}], get, []),
        parse_xml(binary_to_list(zlib:gunzip(Body)))
    catch
        _:E ->
            lager:error("xml parsing error ~p~n~p", [E, erlang:get_stacktrace()]),
            error
    end.

parse_xml(Xml) ->
        {Root, _} = xmerl_scan:string(Xml),
        {ok, parse_xml_tree(Root)}.

% rss -> channel -> item -> title
parse_xml_tree(#xmlElement{name=rss, content=Content}) ->
    {Channel, _} = find_element(channel, Content),
    parse_xml_tree_channel(Channel).

parse_xml_tree_channel(#xmlElement{content=Content}) ->
    parse_xml_tree_channel(Content, 5, []).

parse_xml_tree_channel(_, 0, Acc) ->
    Acc;
parse_xml_tree_channel(Content, N, Acc) ->
    {Item, ContentTail} = find_element(item, Content),
    parse_xml_tree_channel(ContentTail, N - 1, [parse_xml_tree_item(Item)|Acc]).

parse_xml_tree_item(#xmlElement{content=Content}) ->
    {#xmlElement{content=[Text|_]}, _} = find_element(title, Content),
    Text#xmlText.value.

find_element(_, []) ->
    not_found;
find_element(RequestedName, [E=#xmlElement{name=Name}|T]) when RequestedName =:= Name ->
    {E, T};
find_element(RequestedName, [_|T]) ->
    find_element(RequestedName, T).

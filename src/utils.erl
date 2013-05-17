-module(utils).

-export([
    start_application/1,
    start_dependencies/1,
    test_application_start/1,
    test_application_stop/1
]).

start_application(AppName) ->
    case application:start(AppName) of
        ok -> 
            ok;
        {error, {already_started, AppName}} -> 
            ok;
        {error, {not_started, _}} -> 
            start_dependencies(AppName),
            application:start(AppName);
        Error ->
            Error
    end.

start_dependencies(AppName) ->
    Live = [ A || {A, _, _} <- application:which_applications() ],
    Deps = lists:reverse(gather_deps(AppName, []) -- [AppName | Live]),
    [ ok = application:start(Dep) || Dep <- Deps ],
    Deps.

gather_deps(AppName, Acc) ->
    case lists:member(AppName, Acc) of
        true -> 
            Acc;
        _    ->
            application:load(AppName),
            case application:get_key(AppName, applications) of
                {ok, DepsList} -> [AppName | lists:foldl(fun gather_deps/2, Acc, DepsList)];
                _              -> [AppName | Acc]
            end
    end.

test_application_start(App) ->
    Deps = start_dependencies(App),
    ok = application:start(App),
    Deps ++ [App].

test_application_stop(Apps) ->
    [application:stop(App) || App <- lists:reverse(Apps)], ok.

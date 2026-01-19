-module(cquill_dev_ffi).
-export([get_dev_server/0, set_dev_server/1, clear_dev_server/0, get_env/1]).

%% Key used in persistent_term for storing the dev mode server
-define(DEV_SERVER_KEY, cquill_dev_server).

%% Get the dev mode server subject from persistent_term
-spec get_dev_server() -> {ok, pid()} | {error, nil}.
get_dev_server() ->
    try persistent_term:get(?DEV_SERVER_KEY) of
        Server -> {ok, Server}
    catch
        error:badarg -> {error, nil}
    end.

%% Store the dev mode server subject in persistent_term
-spec set_dev_server(pid()) -> nil.
set_dev_server(Server) ->
    persistent_term:put(?DEV_SERVER_KEY, Server),
    nil.

%% Clear the dev mode server from persistent_term
-spec clear_dev_server() -> nil.
clear_dev_server() ->
    try persistent_term:erase(?DEV_SERVER_KEY) of
        _ -> nil
    catch
        error:badarg -> nil
    end.

%% Get an environment variable
-spec get_env(binary()) -> {ok, binary()} | {error, nil}.
get_env(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.

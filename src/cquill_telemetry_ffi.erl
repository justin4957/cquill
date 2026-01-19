-module(cquill_telemetry_ffi).
-export([get_server/0, set_server/1, clear_server/0, now_us/0]).

%% Key used in persistent_term for storing the telemetry server
-define(SERVER_KEY, cquill_telemetry_server).

%% Get the telemetry server subject from persistent_term
-spec get_server() -> {ok, pid()} | {error, nil}.
get_server() ->
    try persistent_term:get(?SERVER_KEY) of
        Server -> {ok, Server}
    catch
        error:badarg -> {error, nil}
    end.

%% Store the telemetry server subject in persistent_term
-spec set_server(pid()) -> nil.
set_server(Server) ->
    persistent_term:put(?SERVER_KEY, Server),
    nil.

%% Clear the telemetry server from persistent_term
-spec clear_server() -> nil.
clear_server() ->
    try persistent_term:erase(?SERVER_KEY) of
        _ -> nil
    catch
        error:badarg -> nil
    end.

%% Get current monotonic time in microseconds
-spec now_us() -> integer().
now_us() ->
    erlang:convert_time_unit(erlang:monotonic_time(), native, microsecond).

-module(bench_ffi).
-export([monotonic_time_us/0]).

%% Get monotonic time in microseconds for benchmarking
monotonic_time_us() ->
    erlang:monotonic_time(microsecond).

-module(cquill_ffi).
-export([coerce_value/1, escape_sql_string/1, safe_tuple_size/1, tuple_to_list/1]).

%% Coerce a Gleam value to a tagged union for type detection
%% This allows the query builder to create the appropriate Value type
coerce_value(Value) when is_integer(Value) ->
    {int_val, Value};
coerce_value(Value) when is_float(Value) ->
    {float_val, Value};
coerce_value(Value) when is_binary(Value) ->
    {string_val, Value};
coerce_value(true) ->
    {bool_val, true};
coerce_value(false) ->
    {bool_val, false};
coerce_value(nil) ->
    nil_val;
coerce_value(none) ->
    nil_val;
coerce_value(Value) when is_list(Value) ->
    {list_val, [coerce_value(V) || V <- Value]};
coerce_value(_) ->
    unknown_val.

%% Escape single quotes in SQL strings by doubling them
escape_sql_string(String) when is_binary(String) ->
    binary:replace(String, <<"'">>, <<"''">>, [global]).

%% Safely get the size of a tuple, returns -1 if not a tuple
safe_tuple_size(Value) when is_tuple(Value) ->
    erlang:tuple_size(Value);
safe_tuple_size(_) ->
    -1.

%% Convert a value to a list suitable for row representation
%% - Tuples are converted using erlang:tuple_to_list/1
%% - Lists are returned as-is (in case pog already converted)
%% - Other values are wrapped in a single-element list
tuple_to_list(Value) when is_tuple(Value) ->
    erlang:tuple_to_list(Value);
tuple_to_list(Value) when is_list(Value) ->
    Value;
tuple_to_list(Value) ->
    [Value].

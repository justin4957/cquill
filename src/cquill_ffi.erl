-module(cquill_ffi).
-export([coerce_value/1]).

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

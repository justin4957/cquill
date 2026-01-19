-module(cquill_crypto_ffi).
-export([hash_sha256/1]).

%% Hash a string using SHA-256
hash_sha256(String) when is_binary(String) ->
    crypto:hash(sha256, String).

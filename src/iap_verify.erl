%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc iap_verify.

-module(iap_verify).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the iap_verify server.
start() ->
    iap_verify_deps:ensure(),
    ensure_started(crypto),
    application:start(iap_verify).


%% @spec stop() -> ok
%% @doc Stop the iap_verify server.
stop() ->
    application:stop(iap_verify).

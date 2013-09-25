%% @author Mochi Media <dev@mochimedia.com>
%% @copyright iap_verify Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the iap_verify application.

-module(iap_verify_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for iap_verify.
start(_Type, _StartArgs) ->
    iap_verify_deps:ensure(),
    iap_verify_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for iap_verify.
stop(_State) ->
    ok.

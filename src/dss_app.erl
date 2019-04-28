%%%-------------------------------------------------------------------
%% @doc dss public API
%% @end
%%%-------------------------------------------------------------------

-module(dss_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    start_mongodb(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/dss/sample", d_webui_sample, element}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 80}], #{
        env => #{dispatch => Dispatch}
    }),
    dss_sup:start_link().

start_mongodb() ->
    application:ensure_all_started(mongodb).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

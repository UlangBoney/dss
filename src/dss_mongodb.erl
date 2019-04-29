-module(dss_mongodb).
-export([
  lookup/4
]).

-include_lib("mongodb/include/mongo_protocol.hrl").
-include_lib("mongodb/include/mongo_types.hrl").

-type resource() :: dss_sample:sample()
                  | dss_classes:class()
                  .


-spec auth_admin(database()) -> connection().
auth_admin(DB) ->
    {ok, Host} = application:get_env(mongodb_config, hostname),
    {ok, Port} = application:get_env(mongodb_config, port),
    {ok, User} = application:get_env(mongodb_config, username),
    {ok, Pass} = application:get_env(mongodb_config, password),
    {ok, Conn} = mc_worker_api:connect([
                    {auth_source, <<"admin">>}
                  , {database   , DB}
                  , {login      , binary:list_to_bin(User)}
                  , {password   , binary:list_to_bin(Pass)}
                  , {host       , Host}
                  , {port       , Port}]),
    Conn.


-spec cursor(database(), collection(), [] | [binary()], fun()) -> [resource()].
cursor(DB, Coll, Args, Fun) ->
    Conn = auth_admin(DB),
    case mc_worker_api:find(Conn, Coll, Args) of
        {ok, Cursor} -> List = mc_cursor:rest(Cursor),
                        [ Fun(Next) || Next <- List ];
        []           -> []
end.


-spec lookup(database(), collection(), selector(), fun()) -> dss_maybe:maybe(resource()).
lookup(DB, Coll, Selector, Fun) ->
    Conn = auth_admin(DB),
    case mc_worker_api:find_one(Conn, Coll, Selector) of
        undefined -> none;
        Map       -> {value, Fun(Map)}
    end.


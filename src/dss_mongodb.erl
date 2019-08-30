-module(dss_mongodb).
-export([
    start_mongodb/0
  , cursor/5
  , lookup/5
]).

-include_lib("mongodb/include/mongo_protocol.hrl").
-include_lib("mongodb/include/mongo_types.hrl").


-spec start_mongodb() -> ok.
start_mongodb() ->
    {ok, [bson, poolboy, pbkdf2, mongodb]} = application:ensure_all_started(mongodb),
    ok.


-spec auth_admin(database()) -> connection().
auth_admin(DB) ->
    {ok, Host} = application:get_env(mongodb, hostname),
    {ok, Port} = application:get_env(mongodb, port),
    {ok, User} = application:get_env(mongodb, username),
    {ok, Pass} = application:get_env(mongodb, password),
    {ok, Conn} = mc_worker_api:connect([
                    {auth_source, <<"admin">>}
                  , {database   , DB}
                  , {login      , binary:list_to_bin(User)}
                  , {password   , binary:list_to_bin(Pass)}
                  , {host       , Host}
                  , {port       , Port}]),
    Conn.


-spec cursor(
        database()
      , collection()
      , [] | [binary()]
      , fun()
      , dss_equipment:equipment_type() | classes | weqpon | shield
    ) -> [dss_equipment:equipment()].
cursor(DB, Coll, Args, Fun, Type) ->
    Conn = auth_admin(DB),
    case mc_worker_api:find(Conn, Coll, Args) of
        {ok, Cursor} -> List = mc_cursor:rest(Cursor),
                        [ Fun(Next, Type) || Next <- List ];
        []           -> []
    end.


-spec lookup(
        database()
      , collection()
      , selector()
      , fun()
      , dss_equipment:equipment_type() | classes | weqpon | shield
    ) -> dss_maybe:maybe(dss_equipment:equipment()).
lookup(DB, Coll, Selector, Fun, Type) ->
    Conn = auth_admin(DB),
    case mc_worker_api:find_one(Conn, Coll, Selector) of
        undefined -> none;
        Map       -> {value, Fun(Map, Type)}
    end.


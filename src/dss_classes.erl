-module(dss_classes).
-export_type([
    id/0
  , class/0
]).
-export([
    % * Read
    list/0
  , lookup/1
  , get/1

    % * Getters
  , id/1
  , name/1
  , levels/1
  , vitality/1
  , attunement/1
  , endurance/1
  , strength/1
  , dexterity/1
  , resistance/1
  , intelligence/1
  , faith/1
]).

-include("dss_error.hrl").

-type id() :: pos_integer().
-opaque class() ::
    #{ id           => id()
     , name         => unicode:unicode_binary()
     , levels       => pos_integer()
     , vitality     => pos_integer()
     , attunement   => pos_integer()
     , endurance    => pos_integer()
     , strength     => pos_integer()
     , dexterity    => pos_integer()
     , resistance   => pos_integer()
     , intelligence => pos_integer()
     , faith        => pos_integer()
    }.


-spec list() -> [class()].
list() ->
    DB   = <<"dss_master">>,
    Coll = <<"classes">>,
    dss_mongodb:cursor(DB, Coll, [], fun from_mongo_map/1).


-spec lookup(id()) -> dss_maybe:maybe(class()).
lookup(ClassID) ->
    DB   = <<"dss_master">>,
    Coll = <<"classes">>,
    dss_mongodb:lookup(DB, Coll, {<<"_id">>, ClassID}, fun from_mongo_map/1).


-spec get(id()) -> class().
get(ClassID) ->
    case lookup(ClassID) of
        {value, Class} ->
            Class;
        none ->
            dss_error:raise(?DSS_CLASS_NOT_FOUND)
    end.


-spec id(class()) -> id().
id(Class) -> maps:get(id, Class).


-spec name(class()) -> unicode:unicode_binary().
name(Class) -> maps:get(name, Class).


-spec levels(class()) -> pos_integer().
levels(Class) -> maps:get(levels, Class).


-spec vitality(class()) -> pos_integer().
vitality(Class) -> maps:get(vitality, Class).


-spec attunement(class()) -> pos_integer().
attunement(Class) -> maps:get(attunement, Class).


-spec endurance(class()) -> pos_integer().
endurance(Class) -> maps:get(endurance, Class).


-spec strength(class()) -> pos_integer().
strength(Class) -> maps:get(strength, Class).


-spec dexterity(class()) -> pos_integer().
dexterity(Class) -> maps:get(dexterity, Class).


-spec resistance(class()) -> pos_integer().
resistance(Class) -> maps:get(resistance, Class).


-spec intelligence(class()) -> pos_integer().
intelligence(Class) -> maps:get(intelligence, Class).


-spec faith(class()) -> pos_integer().
faith(Class) -> maps:get(faith, Class).


-spec from_mongo_map(map()) -> class().
from_mongo_map(MongoMap) ->
    #{ id           => maps:get(<<"_id">>          , MongoMap)
     , name         => maps:get(<<"name">>        , MongoMap)
     , levels       => maps:get(<<"levels">>      , MongoMap)
     , vitality     => maps:get(<<"vitality">>    , MongoMap)
     , attunement   => maps:get(<<"attunement">>  , MongoMap)
     , endurance    => maps:get(<<"endurance">>   , MongoMap)
     , strength     => maps:get(<<"strength">>    , MongoMap)
     , dexterity    => maps:get(<<"dexterity">>   , MongoMap)
     , resistance   => maps:get(<<"resistance">>  , MongoMap)
     , intelligence => maps:get(<<"intelligence">>, MongoMap)
     , faith        => maps:get(<<"faith">>       , MongoMap)
    }.


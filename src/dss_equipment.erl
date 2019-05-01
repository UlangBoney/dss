-module(dss_equipment).
-export_type([
    id/0
  , equipment/0
]).
-export([
    % * Read
    list/1
  , lookup/2
  , get/2

    % * Getters
  , id/1
  , name/1
  , weight/1
  , requirements/1
]).

-include("dss_error.hrl").

-type id() :: pos_integer().
-type equipment() :: dagger()
                  |  straight_sword()
                  |  small_shield()
                  .
-type equipment_type() :: weapon
                        | shield
                        .
-type detail() :: dagger.
-type dagger() ::
    #{ id           => id()
     , name         => #{ english  => unicode:unicode_binary()
                        , japanese => unicode:unicode_binary()}
     , weight       => float()
     , requirements => requirements()
    }.
-type straight_sword() ::
    #{ id           => id()
     , name         => #{ english  => unicode:unicode_binary()
                        , japanese => unicode:unicode_binary()}
     , weight       => float()
     , requirements => requirements()
    }.
-type small_shield() ::
    #{ id           => id()
     , name         => #{ english  => unicode:unicode_binary()
                        , japanese => unicode:unicode_binary()}
     , weight       => float()
     , requirements => requirements()
    }.
-type requirements() ::
    #{ strenght     => pos_integer()
     , dexterity    => pos_integer()
     , intelligence => pos_integer()
     , faith        => pos_integer()
    }.


-spec list({equipment_type(), detail()}) -> [equipment()].
list({weapon, Detail}) ->
    DB = <<"dss_master">>,
    case Detail of
        dagger         -> dss_mongodb:cursor(DB, <<"equipment.weapons.daggers">>, [], fun from_mongo_map/2, {weapon, dagger});
        straight_sword -> dss_mongodb:cursor(DB, <<"equipment.weapons.straightSwords">>, [], fun from_mongo_map/2, {weapon, straight_sword});
        _ -> []
    end;
list({shield, Detail}) ->
    DB = <<"dss_master">>,
    case Detail of
        small_shield -> dss_mongodb:cursor(DB, <<"equipment.shields.smallShields">>, [], fun from_mongo_map/2, {shield, small_shield});
        _ -> []
    end.


-spec lookup({equipment_type(), detail()}, id()) -> dss_maybe:maybe(equipment()).
lookup({weapon, Detail}, ID) ->
    DB = <<"dss_master">>,
    case Detail of
        dagger -> dss_mongodb:lookup(DB, <<"equipment.weapons.daggers">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, dagger});
        straight_sword -> dss_mongodb:lookup(DB, <<"equipment.weapons.straightSwords">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, straight_sword});
        _ -> none
    end;
lookup({shield, Detail}, ID) ->
    DB = <<"dss_master">>,
    case Detail of
        small_shield -> dss_mongodb:lookup(DB, <<"equipment.shields.smallShields">>, {<<"_id">>, ID}, fun from_mongo_map/2, {shield, small_shield});
        _            -> none
    end.


-spec get({equipment_type(), detail()}, id()) -> equipment().
get({EqpType, Detail}, ID) ->
    case lookup({EqpType, Detail}, ID) of
        {value, Equipment} -> Equipment;
        none -> dss_error:raise(?DSS_NOT_FOUND)
    end.


-spec id(equipment()) -> id().
id(Equipment) -> maps:get(id, Equipment).


-spec name(equipment()) -> unicode:unicode_binary().
name(Equipment) -> maps:get(name, Equipment).


-spec weight(equipment()) -> float().
weight(Equipment) -> maps:get(weight, Equipment).


-spec requirements(equipment()) -> requirements().
requirements(Equipment) -> maps:get(requirements, Equipment).


-spec from_mongo_map(map(), {equipment_type(), detail()}) -> equipment().
from_mongo_map(MongoMap, {_, Detail})
    when Detail == dagger; Detail == straight_sword; Detail == small_shield ->
    #{ id           => maps:get(<<"_id">>         , MongoMap)
     , name         => maps:get(<<"name">>        , MongoMap)
     , weight       => maps:get(<<"weight">>      , MongoMap)
     , requirements => maps:get(<<"requirements">>, MongoMap)
    }.


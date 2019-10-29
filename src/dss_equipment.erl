-module(dss_equipment).
-export_type([
    id/0
  , weapon/0
  , ring/0
  , armor/0
]).
-export([
    % * Read
    list/1
  , lookup/1
  , get/1

    % * Getters
  , id/1
  , name/1
  , weight/1
  , type/1
  , requirements/1
  , equip_weight_magnification/1
  , attunement_slots/1
]).

-include("dss_error.hrl").

-type id() :: pos_integer().
-type equipment_type() :: weapon_type()
                        | armor_type()
                        | ring
                        .
-type weapon_type() ::    dagger
                        | straight_sword
                        | greats_sword
                        | ultra_greatsword
                        | curved_sword
                        | curved_greatsword
                        | thrusting_sword
                        | katana
                        | axe
                        | greataxe
                        | hammer
                        | great_hammer
                        | spear
                        | long_spear
                        | halberd
                        | whip
                        | fist
                        | bow
                        | greatbow
                        | crossbow
                        | catalyst
                        | pyromancy_flame
                        | talisman
                        | small_shield
                        | normal_shield
                        | large_shield
                        | lanthanum
                        .
-type armor_type() ::     head_armor
                        | chest_armor
                        | hand_armor
                        | leg_armor
                        .
-type weapon() ::
    #{ id            => id()
     , name          => #{ english  => unicode:unicode_binary()
                         , japanese => unicode:unicode_binary()}
     , type          => weapon_type()
     , weight        => float()
     , requirements  => requirements()
    }.
-type requirements() ::
    #{ strenght     => pos_integer()
     , dexterity    => pos_integer()
     , intelligence => pos_integer()
     , faith        => pos_integer()
    }.
-type armor() ::
    #{ id      => id()
     , name    => #{ english  => unicode:unicode_binary()
                   , japanese => unicode:unicode_binary()}
     , type    => armor_type()
     , weight  => float()
     , equipWeightMagnification => dss_maybe:maybe(pos_integer())
    }.
-type ring() ::
    #{ id      => id()
     , name    => #{ english  => unicode:unicode_binary()
                   , japanese => unicode:unicode_binary()}
     , type => ring
     , equipWeightMagnification => dss_maybe:maybe(pos_integer())
     , attunementSlots => dss_maybe:maybe(pos_integer())
    }.


-spec list(equipment_type()) -> [weapon()].
list(EQPType) ->
    DB = <<"dss_master">>,
    {Coll, Query, Fun} = case EQPType of
        EQPType when EQPType == head_armor; EQPType == chest_armor;
                     EQPType == hand_armor; EQPType == leg_armor ->
            {<<"equipment.armor">>,
                {<<"type">>, atom_to_binary(EQPType, utf8)}, fun from_mongo_map_armor/1};

        EQPType when EQPType == ring ->
            {<<"equipment.rings">>,
                {<<"type">>, <<"ring">>}, fun from_mongo_map_ring/1};

        _ ->
            {<<"equipment.weapons">>,
                {<<"type">>, atom_to_binary(EQPType, utf8)}, fun from_mongo_map_weapon/1}
    end,
    dss_mongodb:cursor(DB, Coll, Query, Fun).


-spec lookup(id()) -> dss_maybe:maybe(weapon()).
lookup(ID) ->
    DB = <<"dss_master">>,
    case ID of
        ID when 1 =< ID, ID =< 183 ->
            dss_mongodb:lookup(DB,
                <<"equipment.weapons">>, {<<"_id">>, ID}, fun from_mongo_map_weapon/1);

        ID when 184 =< ID, ID =< 415 ->
            dss_mongodb:lookup(DB,
                <<"equipment.armor">>, {<<"_id">>, ID}, fun from_mongo_map_armor/1);

        ID when 416 =< ID, ID =< 456 ->
            dss_mongodb:lookup(DB,
                <<"equipment.rings">>, {<<"_id">>, ID}, fun from_mongo_map_ring/1);

        _  -> none
    end.


-spec get(id()) -> weapon().
get(ID) ->
    case lookup(ID) of
        {value, Equipment} -> Equipment;
        none -> dss_error:raise(?DSS_NOT_FOUND)
    end.

-spec id(weapon()) -> id().
id(Equipment) -> maps:get(id, Equipment).


-spec name(weapon()) -> #{english := unicode:unicode_binary(), japanese := unicode:unicode_binary()}.
name(Equipment) -> maps:get(name, Equipment).


-spec weight(weapon()) -> float().
weight(Equipment) -> maps:get(weight, Equipment).


-spec type(weapon()) -> equipment_type().
type(Equipment) -> maps:get(type, Equipment).


-spec requirements(weapon()) -> requirements().
requirements(Equipment) -> maps:get(requirements, Equipment).


-spec equip_weight_magnification(weapon()) -> dss_maybe:maybe(pos_integer()).
equip_weight_magnification(Equipment) ->
    maps:get(equipWeightMagnification, Equipment).


-spec attunement_slots(weapon()) -> dss_maybe:maybe(pos_integer()).
attunement_slots(Equipment) ->
    maps:get(attunementSlots, Equipment).


-spec from_mongo_map_ring(map()) -> ring().
from_mongo_map_ring(MongoMap) ->
    #{ id   => maps:get(<<"_id">>, MongoMap)
     , name =>
            case maps:get(<<"name">>, MongoMap) of
                Name -> #{ english  => maps:get(<<"english">> , Name)
                         , japanese => maps:get(<<"japanese">>, Name)}
            end
     , type => maps:get(<<"type">>, MongoMap)
     , equipWeightMagnification =>
            case maps:get(<<"equipWeightMagnification">>, MongoMap) of
                undefined -> none;
                EWM       -> {value, EWM}
            end
     , attunementSlots =>
            case maps:get(<<"attunementSlots">>, MongoMap) of
                undefined       -> none;
                AttunementSlots -> {value, AttunementSlots}
            end
    }.


-spec from_mongo_map_armor(map()) -> armor().
from_mongo_map_armor(MongoMap) ->
    #{ id   => maps:get(<<"_id">>, MongoMap)
     , name =>
            case maps:get(<<"name">>, MongoMap) of
                Name -> #{ english  => maps:get(<<"english">> , Name)
                         , japanese => maps:get(<<"japanese">>, Name)
                        }
            end
     , type   => maps:get(<<"type">>, MongoMap)
     , weight => maps:get(<<"weight">>, MongoMap)
     , equipWeightMagnification =>
            case maps:get(<<"equipWeightMagnification">>, MongoMap) of
                undefined -> none;
                EqpWeight -> {value, EqpWeight}
            end
    }.


-spec from_mongo_map_weapon(map()) -> weapon().
from_mongo_map_weapon(MongoMap) ->
    #{ id   => maps:get(<<"_id">>, MongoMap)
     , name =>
            case maps:get(<<"name">>, MongoMap) of
                Name -> #{ english  => maps:get(<<"english">> , Name),
                           japanese => maps:get(<<"japanese">>, Name)}
            end
     , type   => maps:get(<<"type">>, MongoMap)
     , weight => maps:get(<<"weight">>, MongoMap)
     , requirements =>
            #{ strength => maps:get(<<"strength">>, maps:get(<<"requirements">>, MongoMap))
             , dexterity => maps:get(<<"dexterity">>, maps:get(<<"requirements">>, MongoMap))
             , intelligence => maps:get(<<"intelligence">>, maps:get(<<"requirements">>, MongoMap))
             , faith => maps:get(<<"faith">>, maps:get(<<"requirements">>, MongoMap))
            }
    }.


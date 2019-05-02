-module(dss_equipment).
-export_type([
    id/0
  , equipment/0
  , ring/0
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
  , effects/1
  , equip_weight/1
  , attunement_slots/1
]).

-include("dss_error.hrl").

-type id() :: pos_integer().
-type equipment_type() :: weapon
                        | shield
                        | head_armor
                        | chest_armor
                        | hand_armor
                        | leg_armor
                        | ring
                        .
-type detail() :: dagger
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
                | halberd
                | whip
                | fist
                | bow
                | greatbow
                | crossbow
                | catalyst
                | talisman
                | pyromancy_flame
                | small_shield
                | normal_shield
                | large_shield
                .
-type equipment() ::
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
-type ring() ::
    #{ id              => id()
     , name            => #{ english  => unicode:unicode_binary()
                           , japanese => unicode:unicode_binary()}
     , effects         => #{ english  => unicode:unicode_binary()
                           , japanese => unicode:unicode_binary()}
     , equipWeight     => dss_maybe:maybe(pos_integer())
     , attunementSlots => dss_maybe:maybe(pos_integer())
    }.


-spec list({equipment_type(), detail()}) -> [equipment()].
list({weapon, Detail}) ->
    DB = <<"dss_master">>,
    case Detail of
        dagger            -> dss_mongodb:cursor(DB, <<"equipment.weapons.daggers">>, [], fun from_mongo_map/2, {weapon, dagger});
        straight_sword    -> dss_mongodb:cursor(DB, <<"equipment.weapons.straightSwords">>, [], fun from_mongo_map/2, {weapon, straight_sword});
        greats_sword      -> dss_mongodb:cursor(DB, <<"equipment.weapons.greatswords">>, [], fun from_mongo_map/2, {weapon, greats_sword});
        ultra_greatsword  -> dss_mongodb:cursor(DB, <<"equipment.weapons.ultraGreatsword">>, [], fun from_mongo_map/2, {weapon, ultra_greatsword});
        curved_sword      -> dss_mongodb:cursor(DB, <<"equipment.weapons.curvedSwords">>, [], fun from_mongo_map/2, {weapon, curved_sword});
        curved_greatsword -> dss_mongodb:cursor(DB, <<"equipment.weapons.curvedGreatswords">>, [], fun from_mongo_map/2, {weapon, curved_greatsword});
        thrusting_sword   -> dss_mongodb:cursor(DB, <<"equipment.weapons.thrustingSwords">>, [], fun from_mongo_map/2, {weapon, thrusting_sword});
        katana            -> dss_mongodb:cursor(DB, <<"equipment.weapons.katanas">>, [], fun from_mongo_map/2, {weapon, katana});
        axe               -> dss_mongodb:cursor(DB, <<"equipment.weapons.axes">>, [], fun from_mongo_map/2, {weapon, axe});
        greataxe          -> dss_mongodb:cursor(DB, <<"equipment.weapons.greataxes">>, [], fun from_mongo_map/2, {weapon, greataxe});
        hammer            -> dss_mongodb:cursor(DB, <<"equipment.weapons.hammers">>, [], fun from_mongo_map/2, {weapon, hammer});
        great_hammer      -> dss_mongodb:cursor(DB, <<"equipment.weapons.greatHammers">>, [], fun from_mongo_map/2, {weapon, great_hammer});
        spear             -> dss_mongodb:cursor(DB, <<"equipment.weapons.spears">>, [], fun from_mongo_map/2, {weapon, spear});
        halberd           -> dss_mongodb:cursor(DB, <<"equipment.weapons.halberds">>, [], fun from_mongo_map/2, {weapon, halberd});
        whip              -> dss_mongodb:cursor(DB, <<"equipment.weapons.whips">>, [], fun from_mongo_map/2, {weapon, whip});
        fist              -> dss_mongodb:cursor(DB, <<"equipment.weapons.fists">>, [], fun from_mongo_map/2, {weapon, fist});
        bow               -> dss_mongodb:cursor(DB, <<"equipment.weapons.bows">>, [], fun from_mongo_map/2, {weapon, bow});
        crossbow          -> dss_mongodb:cursor(DB, <<"equipment.weapons.crossbows">>, [], fun from_mongo_map/2, {weapon, crossbow});
        catalyst          -> dss_mongodb:cursor(DB, <<"equipment.weapons.catalysts">>, [], fun from_mongo_map/2, {weapon, catalyst});
        talisman          -> dss_mongodb:cursor(DB, <<"equipment.weapons.talismans">>, [], fun from_mongo_map/2, {weapon, talisman});
        pyromancy_flame   -> dss_mongodb:cursor(DB, <<"equipment.weapons.pyromancyFlames">>, [], fun from_mongo_map/2, {weapon, pyromancy_flame});
        _ -> []
    end;
list({shield, Detail}) ->
    DB = <<"dss_master">>,
    case Detail of
        small_shield  -> dss_mongodb:cursor(DB, <<"equipment.shields.smallShields">>, [], fun from_mongo_map/2, {shield, small_shield});
        normal_shield -> dss_mongodb:cursor(DB, <<"equipment.shields.normalShields">>, [], fun from_mongo_map/2, {shield, normal_shield});
        large_shield  -> dss_mongodb:cursor(DB, <<"equipment.shields.largeShields">>, [], fun from_mongo_map/2, {shield, large_shield});
        _ -> []
    end;
list({EqpType, none}) ->
    DB = <<"dss_master">>,
    case EqpType of
        head_armor  -> dss_mongodb:cursor(DB, <<"equipment.headArmor">>, [], fun from_mongo_map/2, {head_armor, none});
        chest_armor -> dss_mongodb:cursor(DB, <<"equipment.chestArmor">>, [], fun from_mongo_map/2, {chest_armor, none});
        hand_armor  -> dss_mongodb:cursor(DB, <<"equipment.handArmor">>, [], fun from_mongo_map/2, {hand_armor, none});
        leg_armor   -> dss_mongodb:cursor(DB, <<"equipment.legArmor">>, [], fun from_mongo_map/2, {leg_armor, none});
        ring        -> dss_mongodb:cursor(DB, <<"equipment.rings">>, [], fun from_mongo_map/2, {ring, none});
        _           -> []
    end.


-spec lookup({equipment_type(), detail()}, id()) -> dss_maybe:maybe(equipment()).
lookup({weapon, Detail}, ID) ->
    DB = <<"dss_master">>,
    case Detail of
        dagger            -> dss_mongodb:lookup(DB, <<"equipment.weapons.daggers">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, dagger});
        straight_sword    -> dss_mongodb:lookup(DB, <<"equipment.weapons.straightSwords">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, straight_sword});
        greats_sword      -> dss_mongodb:lookup(DB, <<"equipment.weapons.greatswords">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, greats_sword});
        ultra_greatsword  -> dss_mongodb:lookup(DB, <<"equipment.weapons.ultraGreatsword">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, ultra_greatsword});
        curved_sword      -> dss_mongodb:lookup(DB, <<"equipment.weapons.curvedSwords">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, curved_sword});
        curved_greatsword -> dss_mongodb:lookup(DB, <<"equipment.weapons.curvedGreatswords">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, curved_greatsword});
        thrusting_sword   -> dss_mongodb:lookup(DB, <<"equipment.weapons.thrustingSwords">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, thrusting_sword});
        katana            -> dss_mongodb:lookup(DB, <<"equipment.weapons.katanas">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, katana});
        axe               -> dss_mongodb:lookup(DB, <<"equipment.weapons.axes">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, axe});
        greataxe          -> dss_mongodb:lookup(DB, <<"equipment.weapons.greataxes">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, greataxe});
        hammer            -> dss_mongodb:lookup(DB, <<"equipment.weapons.hammers">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, hammer});
        great_hammer      -> dss_mongodb:lookup(DB, <<"equipment.weapons.greatHammers">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, great_hammer});
        spear             -> dss_mongodb:lookup(DB, <<"equipment.weapons.spears">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, spear});
        halberd           -> dss_mongodb:lookup(DB, <<"equipment.weapons.halberds">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, halberd});
        whip              -> dss_mongodb:lookup(DB, <<"equipment.weapons.whips">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, whip});
        fist              -> dss_mongodb:lookup(DB, <<"equipment.weapons.fists">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, fist});
        bow               -> dss_mongodb:lookup(DB, <<"equipment.weapons.bows">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, bow});
        crossbow          -> dss_mongodb:lookup(DB, <<"equipment.weapons.crossbows">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, crossbow});
        catalyst          -> dss_mongodb:lookup(DB, <<"equipment.weapons.catalysts">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, catalyst});
        talisman          -> dss_mongodb:lookup(DB, <<"equipment.weapons.talismans">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, talisman});
        pyromancy_flame   -> dss_mongodb:lookup(DB, <<"equipment.weapons.pyromancyFlames">>, {<<"_id">>, ID}, fun from_mongo_map/2, {weapon, pyromancy_flame});
        _ -> none
    end;
lookup({shield, Detail}, ID) ->
    DB = <<"dss_master">>,
    case Detail of
        small_shield  -> dss_mongodb:lookup(DB, <<"equipment.shields.smallShields">>, {<<"_id">>, ID}, fun from_mongo_map/2, {shield, small_shield});
        normal_shield -> dss_mongodb:lookup(DB, <<"equipment.shields.normalShields">>, {<<"_id">>, ID}, fun from_mongo_map/2, {shield, normal_shield});
        large_shield  -> dss_mongodb:lookup(DB, <<"equipment.shields.largeShields">>, {<<"_id">>, ID}, fun from_mongo_map/2, {shield, large_shield});
        _             -> none
    end;
lookup({EqpType, none}, ID) ->
    DB = <<"dss_master">>,
    case EqpType of
        head_armor  -> dss_mongodb:lookup(DB, <<"equipment.headArmor">>, {<<"_id">>, ID}, fun from_mongo_map/2, {head_armor, none});
        chest_armor -> dss_mongodb:lookup(DB, <<"equipment.chestArmor">>, {<<"_id">>, ID}, fun from_mongo_map/2, {chest_armor, none});
        hand_armor  -> dss_mongodb:lookup(DB, <<"equipment.handArmor">>, {<<"_id">>, ID}, fun from_mongo_map/2, {hand_armor, none});
        leg_armor   -> dss_mongodb:lookup(DB, <<"equipment.legArmor">>, {<<"_id">>, ID}, fun from_mongo_map/2, {legd_armor, none});
        ring        -> dss_mongodb:lookup(DB, <<"equipment.rings">>, {<<"_id">>, ID}, fun from_mongo_map/2, {ring, none});
        _           -> none
    end.


-spec get({equipment_type(), detail()}, id()) -> equipment().
get({EqpType, Detail}, ID) ->
    case lookup({EqpType, Detail}, ID) of
        {value, Equipment} -> Equipment;
        none -> dss_error:raise(?DSS_NOT_FOUND)
    end.


-spec id(equipment()) -> id().
id(Equipment) -> maps:get(id, Equipment).


-spec name(equipment()) -> #{english := unicode:unicode_binary(), japanese := unicode:unicode_binary()}.
name(Equipment) -> maps:get(name, Equipment).


-spec weight(equipment()) -> float().
weight(Equipment) -> maps:get(weight, Equipment).


-spec requirements(equipment()) -> requirements().
requirements(Equipment) -> maps:get(requirements, Equipment).


-spec effects(equipment()) -> #{english := unicode:unicode_binary(), japanese := unicode:unicode_binary()}.
effects(Equipment) -> maps:get(effects, Equipment).


-spec equip_weight(equipment()) -> dss_maybe:maybe(pos_integer()).
equip_weight(Equipment) ->
    maps:get(equipWeight, Equipment).


-spec attunement_slots(equipment()) -> dss_maybe:maybe(pos_integer()).
attunement_slots(Equipment) ->
    maps:get(attunementSlots, Equipment).


-spec from_mongo_map(map(), {equipment_type(), detail() | none}) -> equipment().
from_mongo_map(MongoMap, {ring, none}) ->
    #{ id          => maps:get(<<"_id">>            , MongoMap)
     , name        => maps:get(<<"name">>           , MongoMap)
     , effects     => maps:get(<<"effects">>        , MongoMap)
     , equipWeight =>
            case maps:get(<<"equipWeight">>, MongoMap) of
                undefined -> none;
                EqpWeight -> EqpWeight
            end
     , attunementSlots =>
            case maps:get(<<"attunementSlots">>, MongoMap) of
                undefined       -> none;
                AttunementSlots -> AttunementSlots
            end
    };
from_mongo_map(MongoMap, {_, none}) ->
    #{ id     => maps:get(<<"_id">>   , MongoMap)
     , name   => maps:get(<<"name">>  , MongoMap)
     , weight => maps:get(<<"weight">>, MongoMap)
    };
from_mongo_map(MongoMap, _) ->
    #{ id           => maps:get(<<"_id">>         , MongoMap)
     , name         => maps:get(<<"name">>        , MongoMap)
     , weight       => maps:get(<<"weight">>      , MongoMap)
     , requirements => maps:get(<<"requirements">>, MongoMap)
    }.


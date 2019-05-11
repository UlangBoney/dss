-module(dss_equipment).
-export_type([
    id/0
  , equipment/0
  , ring/0
  , head_armor/0
  , armor/0
]).
-export([
    % * Read
    list/1
  , lookup/2
  , get/2
  , equipment_type/1

    % * Getters
  , id/1
  , name/1
  , weight/1
  , requirements/1
  , equip_weight_magnification/1
  , attunement_slots/1
]).

-include("dss_error.hrl").

-type id() :: pos_integer().
-type equipment_type() :: dagger
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
                        | talisman
                        | pyromancy_flame
                        | small_shield
                        | normal_shield
                        | large_shield
                        | lanthanum
                        | head_armor
                        | chest_armor
                        | hand_armor
                        | leg_armor
                        | ring
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
    #{ id      => id()
     , name    => #{ english  => unicode:unicode_binary()
                   , japanese => unicode:unicode_binary()}
     , equipWeightMagnification => dss_maybe:maybe(pos_integer())
     , attunementSlots => dss_maybe:maybe(pos_integer())
    }.
-type head_armor() ::
    #{ id      => id()
     , name    => #{ english  => unicode:unicode_binary()
                   , japanese => unicode:unicode_binary()}
     , weight  => float()
     , equipWeightMagnification => dss_maybe:maybe(pos_integer())
    }.
-type armor() ::
    #{ id     => id()
     , name   => #{ english  => unicode:unicode_binary()
                  , japanese => unicode:unicode_binary()}
     , weight => float()
    }.


-spec list(equipment_type()) -> [equipment()].
list(Detail) ->
    DB = <<"dss_master">>,
    case Detail of
        dagger            -> dss_mongodb:cursor(DB, <<"equipment.weapons.daggers">>, [], fun from_mongo_map/2, weapon);
        straight_sword    -> dss_mongodb:cursor(DB, <<"equipment.weapons.straightSwords">>, [], fun from_mongo_map/2, weapon);
        greats_sword      -> dss_mongodb:cursor(DB, <<"equipment.weapons.greatswords">>, [], fun from_mongo_map/2, weapon);
        ultra_greatsword  -> dss_mongodb:cursor(DB, <<"equipment.weapons.ultraGreatsword">>, [], fun from_mongo_map/2, weapon);
        curved_sword      -> dss_mongodb:cursor(DB, <<"equipment.weapons.curvedSwords">>, [], fun from_mongo_map/2, weapon);
        curved_greatsword -> dss_mongodb:cursor(DB, <<"equipment.weapons.curvedGreatswords">>, [], fun from_mongo_map/2, weapon);
        thrusting_sword   -> dss_mongodb:cursor(DB, <<"equipment.weapons.thrustingSwords">>, [], fun from_mongo_map/2, weapon);
        katana            -> dss_mongodb:cursor(DB, <<"equipment.weapons.katanas">>, [], fun from_mongo_map/2, weapon);
        axe               -> dss_mongodb:cursor(DB, <<"equipment.weapons.axes">>, [], fun from_mongo_map/2, weapon);
        greataxe          -> dss_mongodb:cursor(DB, <<"equipment.weapons.greataxes">>, [], fun from_mongo_map/2, weapon);
        hammer            -> dss_mongodb:cursor(DB, <<"equipment.weapons.hammers">>, [], fun from_mongo_map/2, weapon);
        great_hammer      -> dss_mongodb:cursor(DB, <<"equipment.weapons.greatHammers">>, [], fun from_mongo_map/2, weapon);
        spear             -> dss_mongodb:cursor(DB, <<"equipment.weapons.spears">>, [], fun from_mongo_map/2, weapon);
        long_spear        -> dss_mongodb:cursor(DB, <<"equipment.weapons.longSpear">>, [], fun from_mongo_map/2, weapon);
        halberd           -> dss_mongodb:cursor(DB, <<"equipment.weapons.halberds">>, [], fun from_mongo_map/2, weapon);
        whip              -> dss_mongodb:cursor(DB, <<"equipment.weapons.whips">>, [], fun from_mongo_map/2, weapon);
        fist              -> dss_mongodb:cursor(DB, <<"equipment.weapons.fists">>, [], fun from_mongo_map/2, weapon);
        bow               -> dss_mongodb:cursor(DB, <<"equipment.weapons.bows">>, [], fun from_mongo_map/2, weapon);
        greatbow          -> dss_mongodb:cursor(DB, <<"equipment.weapons.greatbows">>, [], fun from_mongo_map/2, weapon);
        crossbow          -> dss_mongodb:cursor(DB, <<"equipment.weapons.crossbows">>, [], fun from_mongo_map/2, weapon);
        catalyst          -> dss_mongodb:cursor(DB, <<"equipment.weapons.catalysts">>, [], fun from_mongo_map/2, weapon);
        talisman          -> dss_mongodb:cursor(DB, <<"equipment.weapons.talismans">>, [], fun from_mongo_map/2, weapon);
        pyromancy_flame   -> dss_mongodb:cursor(DB, <<"equipment.weapons.pyromancyFlames">>, [], fun from_mongo_map/2, weapon);
        small_shield      -> dss_mongodb:cursor(DB, <<"equipment.shields.smallShields">>, [], fun from_mongo_map/2, shield);
        normal_shield     -> dss_mongodb:cursor(DB, <<"equipment.shields.normalShields">>, [], fun from_mongo_map/2, shield);
        large_shield      -> dss_mongodb:cursor(DB, <<"equipment.shields.largeShields">>, [], fun from_mongo_map/2, shield);
        lanthanum         -> dss_mongodb:cursor(DB, <<"equipment.lanthanum">>, [], fun from_mongo_map/2, shield);
        head_armor        -> dss_mongodb:cursor(DB, <<"equipment.headArmor">>, [], fun from_mongo_map/2, head_armor);
        chest_armor       -> dss_mongodb:cursor(DB, <<"equipment.chestArmor">>, [], fun from_mongo_map/2, chest_armor);
        hand_armor        -> dss_mongodb:cursor(DB, <<"equipment.handArmor">>, [], fun from_mongo_map/2, hand_armor);
        leg_armor         -> dss_mongodb:cursor(DB, <<"equipment.legArmor">>, [], fun from_mongo_map/2, leg_armor);
        ring              -> dss_mongodb:cursor(DB, <<"equipment.rings">>, [], fun from_mongo_map/2, ring);
        _                 -> []
    end.


-spec lookup(equipment_type(), id()) -> dss_maybe:maybe(equipment()).
lookup(EqpType, ID) ->
    DB = <<"dss_master">>,
    case EqpType of
        dagger            -> dss_mongodb:lookup(DB, <<"equipment.weapons.daggers">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        straight_sword    -> dss_mongodb:lookup(DB, <<"equipment.weapons.straightSwords">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        greats_sword      -> dss_mongodb:lookup(DB, <<"equipment.weapons.greatswords">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        ultra_greatsword  -> dss_mongodb:lookup(DB, <<"equipment.weapons.ultraGreatsword">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        curved_sword      -> dss_mongodb:lookup(DB, <<"equipment.weapons.curvedSwords">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        curved_greatsword -> dss_mongodb:lookup(DB, <<"equipment.weapons.curvedGreatswords">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        thrusting_sword   -> dss_mongodb:lookup(DB, <<"equipment.weapons.thrustingSwords">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        katana            -> dss_mongodb:lookup(DB, <<"equipment.weapons.katanas">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        axe               -> dss_mongodb:lookup(DB, <<"equipment.weapons.axes">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        greataxe          -> dss_mongodb:lookup(DB, <<"equipment.weapons.greataxes">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        hammer            -> dss_mongodb:lookup(DB, <<"equipment.weapons.hammers">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        great_hammer      -> dss_mongodb:lookup(DB, <<"equipment.weapons.greatHammers">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        spear             -> dss_mongodb:lookup(DB, <<"equipment.weapons.spears">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        long_spear        -> dss_mongodb:lookup(DB, <<"equipment.weapons.longSpear">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        halberd           -> dss_mongodb:lookup(DB, <<"equipment.weapons.halberds">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        whip              -> dss_mongodb:lookup(DB, <<"equipment.weapons.whips">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        fist              -> dss_mongodb:lookup(DB, <<"equipment.weapons.fists">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        bow               -> dss_mongodb:lookup(DB, <<"equipment.weapons.bows">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        greatbow          -> dss_mongodb:lookup(DB, <<"equipment.weapons.greatbows">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        crossbow          -> dss_mongodb:lookup(DB, <<"equipment.weapons.crossbows">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        catalyst          -> dss_mongodb:lookup(DB, <<"equipment.weapons.catalysts">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        talisman          -> dss_mongodb:lookup(DB, <<"equipment.weapons.talismans">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        pyromancy_flame   -> dss_mongodb:lookup(DB, <<"equipment.weapons.pyromancyFlames">>, {<<"_id">>, ID}, fun from_mongo_map/2, weapon);
        small_shield      -> dss_mongodb:lookup(DB, <<"equipment.shields.smallShields">>, {<<"_id">>, ID}, fun from_mongo_map/2, shield);
        normal_shield     -> dss_mongodb:lookup(DB, <<"equipment.shields.normalShields">>, {<<"_id">>, ID}, fun from_mongo_map/2, shield);
        large_shield      -> dss_mongodb:lookup(DB, <<"equipment.shields.largeShields">>, {<<"_id">>, ID}, fun from_mongo_map/2, shield);
        lanthanum         -> dss_mongodb:lookup(DB, <<"equipment.lanthanum">>, {<<"_id">>, ID}, fun from_mongo_map/2, shield);
        head_armor        -> dss_mongodb:lookup(DB, <<"equipment.headArmor">>, {<<"_id">>, ID}, fun from_mongo_map/2, head_armor);
        chest_armor       -> dss_mongodb:lookup(DB, <<"equipment.chestArmor">>, {<<"_id">>, ID}, fun from_mongo_map/2, chest_armor);
        hand_armor        -> dss_mongodb:lookup(DB, <<"equipment.handArmor">>, {<<"_id">>, ID}, fun from_mongo_map/2, hand_armor);
        leg_armor         -> dss_mongodb:lookup(DB, <<"equipment.legArmor">>, {<<"_id">>, ID}, fun from_mongo_map/2, legd_armor);
        ring              -> dss_mongodb:lookup(DB, <<"equipment.rings">>, {<<"_id">>, ID}, fun from_mongo_map/2, ring);
        _                 -> none
    end.


-spec get(equipment_type(), id()) -> equipment().
get(EqpType, ID) ->
    case lookup(EqpType, ID) of
        {value, Equipment} -> Equipment;
        none -> dss_error:raise(?DSS_NOT_FOUND)
    end.


-spec equipment_type(pos_integer()) -> equipment_type().
equipment_type(ID) when 1   =< ID, ID =< 6   -> dagger;
equipment_type(ID) when 7   =< ID, ID =< 19  -> short_sword;
equipment_type(ID) when 20  =< ID, ID =< 32  -> greatsword;
equipment_type(ID) when 33  =< ID, ID =< 37  -> ultra_greatsword;
equipment_type(ID) when 38  =< ID, ID =< 44  -> curved_sword;
equipment_type(ID) when 45  =< ID, ID =< 47  -> curved_greatsword;
equipment_type(ID) when 48  =< ID, ID =< 52  -> thrusting_sword;
equipment_type(ID) when 53  =< ID, ID =< 56  -> katana;
equipment_type(ID) when 57  =< ID, ID =< 62  -> axe;
equipment_type(ID) when 63  =< ID, ID =< 67  -> greataxe;
equipment_type(ID) when 68  =< ID, ID =< 76  -> hammer;
equipment_type(ID) when 77  =< ID, ID =< 82  -> great_hammer;
equipment_type(ID) when 83  =< ID, ID =< 91  -> spear;
equipment_type(ID) when ID == 92             -> long_spear;
equipment_type(ID) when 93  =< ID, ID =< 101 -> halberd;
equipment_type(ID) when 102 =< ID, ID =< 104 -> whip;
equipment_type(ID) when 105 =< ID, ID =< 108 -> fist;
equipment_type(ID) when 109 =< ID, ID =< 113 -> bow;
equipment_type(ID) when 114 =< ID, ID =< 115 -> greatbow;
equipment_type(ID) when 116 =< ID, ID =< 119 -> crossbow;
equipment_type(ID) when 120 =< ID, ID =< 130 -> catalyst;
equipment_type(ID) when 131 =< ID, ID =< 132 -> pyromancy_flame;
equipment_type(ID) when 133 =< ID, ID =< 139 -> talisman;
equipment_type(ID) when 140 =< ID, ID =< 150 -> small_shield;
equipment_type(ID) when 151 =< ID, ID =< 173 -> normal_shield;
equipment_type(ID) when 174 =< ID, ID =< 182 -> large_shield;
equipment_type(ID) when ID == 183            -> lanthanum;
equipment_type(ID) when 184 =< ID, ID =< 250 -> head_armor;
equipment_type(ID) when 251 =< ID, ID =< 306 -> chest_armor;
equipment_type(ID) when 307 =< ID, ID =< 359 -> hand_armor;
equipment_type(ID) when 360 =< ID, ID =< 415 -> leg_armor;
equipment_type(ID) when 416 =< ID, ID =< 456 -> ring.


-spec id(equipment()) -> id().
id(Equipment) -> maps:get(id, Equipment).


-spec name(equipment()) -> #{english := unicode:unicode_binary(), japanese := unicode:unicode_binary()}.
name(Equipment) -> maps:get(name, Equipment).


-spec weight(equipment()) -> float().
weight(Equipment) -> maps:get(weight, Equipment).


-spec requirements(equipment()) -> requirements().
requirements(Equipment) -> maps:get(requirements, Equipment).


-spec equip_weight_magnification(equipment()) -> dss_maybe:maybe(pos_integer()).
equip_weight_magnification(Equipment) ->
    maps:get(equipWeightMagnification, Equipment).


-spec attunement_slots(equipment()) -> dss_maybe:maybe(pos_integer()).
attunement_slots(Equipment) ->
    maps:get(attunementSlots, Equipment).


-spec from_mongo_map(map(), equipment_type() | weqpon | shield) -> ring() | head_armor() | equipment() | armor().
from_mongo_map(MongoMap, ring) ->
    #{ id   => maps:get(<<"_id">>, MongoMap)
     , name =>
            case maps:get(<<"name">>, MongoMap) of
                Name -> #{ english  => maps:get(<<"english">> , Name),
                           japanese => maps:get(<<"japanese">>, Name)}
            end
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
    };
from_mongo_map(MongoMap, head_armor) ->
    #{ id   => maps:get(<<"_id">>, MongoMap)
     , name =>
            case maps:get(<<"name">>, MongoMap) of
                Name -> #{ english  => maps:get(<<"english">> , Name),
                           japanese => maps:get(<<"japanese">>, Name)}
            end
     , weight => maps:get(<<"weight">>, MongoMap)
     , equipWeightMagnification =>
            case maps:get(<<"equipWeightMagnification">>, MongoMap) of
                undefined -> none;
                EqpWeight -> {value, EqpWeight}
            end
    };
from_mongo_map(MongoMap, Type) when Type == weapon; Type == shield ->
    #{ id   => maps:get(<<"_id">>, MongoMap)
     , name =>
            case maps:get(<<"name">>, MongoMap) of
                Name -> #{ english  => maps:get(<<"english">> , Name),
                           japanese => maps:get(<<"japanese">>, Name)}
            end
     , weight => maps:get(<<"weight">>, MongoMap)
     , requirements =>
            #{ strength => maps:get(<<"strength">>, maps:get(<<"requirements">>, MongoMap))
             , dexterity => maps:get(<<"dexterity">>, maps:get(<<"requirements">>, MongoMap))
             , intelligence => maps:get(<<"intelligence">>, maps:get(<<"requirements">>, MongoMap))
             , faith => maps:get(<<"faith">>, maps:get(<<"requirements">>, MongoMap))}
    };
from_mongo_map(MongoMap, _) ->
    #{ id   => maps:get(<<"_id">>, MongoMap)
     , name =>
            case maps:get(<<"name">>, MongoMap) of
                Name -> #{ english  => maps:get(<<"english">> , Name),
                           japanese => maps:get(<<"japanese">>, Name)}
            end
     , weight => maps:get(<<"weight">>, MongoMap)
    }.


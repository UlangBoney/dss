-module(dss_equipment_SUITE).
-export([
    all/0
]).
-export([
    equipment_tests/1
]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-spec all() -> [atom()].
all() ->
    [equipment_tests].


-spec equipment_tests(any()) -> ok.
equipment_tests(_Config) ->
    ok = dss_mongodb:start_mongodb(),
    ?assertMatch(ok, lookup_test()),
    ?assertMatch(ok, list_test()),
    ok = application:stop(bson),
    ok = application:stop(poolboy),
    ok = application:stop(pbkdf2),
    ok = application:stop(mongodb).


-spec lookup_test() -> ok.
lookup_test() ->
    ?assertMatch(ok, dagger_lookup_test()),
    ?assertMatch(ok, straight_sword_lookup_test()),
    ?assertMatch(ok, greats_sword_lookup_test()),
    ?assertMatch(ok, ultra_greatsword_lookup_test()),
    ?assertMatch(ok, curved_sword_lookup_test()),
    ?assertMatch(ok, curved_greatsword_lookup_test()),
    ?assertMatch(ok, thrusting_sword_lookup_test()),
    ?assertMatch(ok, katana_lookup_test()),
    ?assertMatch(ok, axe_lookup_test()),
    ?assertMatch(ok, greataxe_lookup_test()),
    ?assertMatch(ok, hammer_lookup_test()),
    ?assertMatch(ok, great_hammer_lookup_test()),
    ?assertMatch(ok, spear_lookup_test()),
    ?assertMatch(ok, long_spear_lookup_test()),
    ?assertMatch(ok, halberd_lookup_test()),
    ?assertMatch(ok, whip_lookup_test()),
    ?assertMatch(ok, fist_lookup_test()),
    ?assertMatch(ok, bow_lookup_test()),
    ?assertMatch(ok, greatbow_lookup_test()),
    ?assertMatch(ok, crossbow_lookup_test()),
    ?assertMatch(ok, catalyst_lookup_test()),
    ?assertMatch(ok, pyromancy_flame_lookup_test()),
    ?assertMatch(ok, talisman_lookup_test()),
    ?assertMatch(ok, small_shield_lookup_test()),
    ?assertMatch(ok, normal_shield_lookup_test()),
    ?assertMatch(ok, large_shield_lookup_test()),
    ?assertMatch(ok, lanthanum_lookup_test()),
    ?assertMatch(ok, head_armor_lookup_test()),
    ?assertMatch(ok, chest_armor_lookup_test()),
    ?assertMatch(ok, hand_armor_lookup_test()),
    ?assertMatch(ok, leg_armor_lookup_test()).


-spec list_test() -> ok.
list_test() ->
    ?assertMatch(ok, dagger_list_test()),
    ?assertMatch(ok, straight_sword_list_test()),
    ?assertMatch(ok, greats_sword_list_test()),
    ?assertMatch(ok, ultra_greatsword_list_test()),
    ?assertMatch(ok, curved_sword_list_test()),
    ?assertMatch(ok, curved_greatsword_list_test()),
    ?assertMatch(ok, thrusting_sword_list_test()),
    ?assertMatch(ok, katana_list_test()),
    ?assertMatch(ok, axe_list_test()),
    ?assertMatch(ok, greataxe_list_test()),
    ?assertMatch(ok, hammer_list_test()),
    ?assertMatch(ok, great_hammer_list_test()),
    ?assertMatch(ok, spear_list_test()),
    ?assertMatch(ok, long_spear_list_test()),
    ?assertMatch(ok, halberd_list_test()),
    ?assertMatch(ok, whip_list_test()),
    ?assertMatch(ok, fist_list_test()),
    ?assertMatch(ok, bow_list_test()),
    ?assertMatch(ok, greatbow_list_test()),
    ?assertMatch(ok, crossbow_list_test()),
    ?assertMatch(ok, catalyst_list_test()),
    ?assertMatch(ok, pyromancy_flame_list_test()),
    ?assertMatch(ok, talisman_list_test()),
    ?assertMatch(ok, small_shield_list_test()),
    ?assertMatch(ok, normal_shield_list_test()),
    ?assertMatch(ok, large_shield_list_test()),
    ?assertMatch(ok, head_armor_list_test()),
    ?assertMatch(ok, chest_armor_list_test()),
    ?assertMatch(ok, hand_armor_list_test()),
    ?assertMatch(ok, leg_armor_list_test()).


-spec dagger_lookup_test() -> ok.
dagger_lookup_test() ->
    none = dss_equipment:lookup(0, dagger),
    none = dss_equipment:lookup(7, dagger),
    ok = lists:foreach(
        fun(EquipmentID) ->
            dagger_lookup_test(EquipmentID)
        end,
        lists:seq(1, 6)
    ).


-spec straight_sword_lookup_test() -> ok.
straight_sword_lookup_test() ->
    none = dss_equipment:lookup(6, straight_sword),
    none = dss_equipment:lookup(20, straight_sword),
    ok = lists:foreach(
        fun(EquipmentID) ->
            straight_sword_lookup_test(EquipmentID)
        end,
        lists:seq(7, 19)
    ).


-spec greats_sword_lookup_test() -> ok.
greats_sword_lookup_test() ->
    none = dss_equipment:lookup(19, greats_sword),
    none = dss_equipment:lookup(33, greats_sword),
    ok = lists:foreach(
        fun(EquipmentID) ->
            greats_sword_lookup_test(EquipmentID)
        end,
        lists:seq(20, 32)
    ).


-spec ultra_greatsword_lookup_test() -> ok.
ultra_greatsword_lookup_test() ->
    none = dss_equipment:lookup(32, ultra_greatsword),
    none = dss_equipment:lookup(38, ultra_greatsword),
    ok = lists:foreach(
        fun(EquipmentID) ->
            ultra_greatsword_lookup_test(EquipmentID)
        end,
        lists:seq(33, 37)
    ).


-spec curved_sword_lookup_test() -> ok.
curved_sword_lookup_test() ->
    none = dss_equipment:lookup(37, curved_sword),
    none = dss_equipment:lookup(45, curved_sword),
    ok = lists:foreach(
        fun(EquipmentID) ->
            curved_sword_lookup_test(EquipmentID)
        end,
        lists:seq(38, 44)
    ).


-spec curved_greatsword_lookup_test() -> ok.
curved_greatsword_lookup_test() ->
    none = dss_equipment:lookup(44, curved_sword),
    none = dss_equipment:lookup(48, curved_sword),
    ok = lists:foreach(
        fun(EquipmentID) ->
            curved_greatsword_lookup_test(EquipmentID)
        end,
        lists:seq(45, 47)
    ).


-spec thrusting_sword_lookup_test() -> ok.
thrusting_sword_lookup_test() ->
    none = dss_equipment:lookup(47, thrusting_sword),
    none = dss_equipment:lookup(53, thrusting_sword),
    ok = lists:foreach(
        fun(EquipmentID) ->
            thrusting_sword_lookup_test(EquipmentID)
        end,
        lists:seq(48, 52)
    ).


-spec katana_lookup_test() -> ok.
katana_lookup_test() ->
    none = dss_equipment:lookup(52, katana),
    none = dss_equipment:lookup(57, katana),
    ok = lists:foreach(
        fun(EquipmentID) ->
            katana_lookup_test(EquipmentID)
        end,
        lists:seq(53, 56)
    ).


-spec axe_lookup_test() -> ok.
axe_lookup_test() ->
    none = dss_equipment:lookup(56, axe),
    none = dss_equipment:lookup(63, axe),
    ok = lists:foreach(
        fun(EquipmentID) ->
            axe_lookup_test(EquipmentID)
        end,
        lists:seq(57, 62)
    ).


-spec greataxe_lookup_test() -> ok.
greataxe_lookup_test() ->
    none = dss_equipment:lookup(62, greataxe),
    none = dss_equipment:lookup(68, greataxe),
    ok = lists:foreach(
        fun(EquipmentID) ->
            greataxe_lookup_test(EquipmentID)
        end,
        lists:seq(63, 67)
    ).


-spec hammer_lookup_test() -> ok.
hammer_lookup_test() ->
    none = dss_equipment:lookup(67, hammer),
    none = dss_equipment:lookup(77, hammer),
    ok = lists:foreach(
        fun(EquipmentID) ->
            hammer_lookup_test(EquipmentID)
        end,
        lists:seq(68, 76)
    ).


-spec great_hammer_lookup_test() -> ok.
great_hammer_lookup_test() ->
    none = dss_equipment:lookup(76, great_hammer),
    none = dss_equipment:lookup(83, great_hammer),
    ok = lists:foreach(
        fun(EquipmentID) ->
            great_hammer_lookup_test(EquipmentID)
        end,
        lists:seq(77, 82)
    ).


-spec spear_lookup_test() -> ok.
spear_lookup_test() ->
    none = dss_equipment:lookup(82, spear),
    none = dss_equipment:lookup(92, spear),
    ok = lists:foreach(
        fun(EquipmentID) ->
            spear_lookup_test(EquipmentID)
        end,
        lists:seq(83, 91)
    ).


-spec long_spear_lookup_test() -> ok.
long_spear_lookup_test() ->
    none = dss_equipment:lookup(91, long_spear),
    none = dss_equipment:lookup(93, long_spear),
    long_spear_lookup_test(92),
    ok.


-spec halberd_lookup_test() -> ok.
halberd_lookup_test() ->
    none = dss_equipment:lookup(92, halberd),
    none = dss_equipment:lookup(102, halberd),
    ok = lists:foreach(
        fun(EquipmentID) ->
            halberd_lookup_test(EquipmentID)
        end,
        lists:seq(93, 101)
    ).


-spec whip_lookup_test() -> ok.
whip_lookup_test() ->
    none = dss_equipment:lookup(101, whip),
    none = dss_equipment:lookup(105, whip),
    ok = lists:foreach(
        fun(EquipmentID) ->
            whip_lookup_test(EquipmentID)
        end,
        lists:seq(102, 104)
    ).


-spec fist_lookup_test() -> ok.
fist_lookup_test() ->
    none = dss_equipment:lookup(104, fist),
    none = dss_equipment:lookup(109, fist),
    ok = lists:foreach(
        fun(EquipmentID) ->
            fist_lookup_test(EquipmentID)
        end,
        lists:seq(105, 108)
    ).


-spec bow_lookup_test() -> ok.
bow_lookup_test() ->
    none = dss_equipment:lookup(108, bow),
    none = dss_equipment:lookup(114, bow),
    ok = lists:foreach(
        fun(EquipmentID) ->
            bow_lookup_test(EquipmentID)
        end,
        lists:seq(109, 113)
    ).


-spec greatbow_lookup_test() -> ok.
greatbow_lookup_test() ->
    none = dss_equipment:lookup(113, greatbow),
    none = dss_equipment:lookup(116, greatbow),
    ok = lists:foreach(
        fun(EquipmentID) ->
            greatbow_lookup_test(EquipmentID)
        end,
        lists:seq(114, 115)
    ).


-spec crossbow_lookup_test() -> ok.
crossbow_lookup_test() ->
    none = dss_equipment:lookup(115, crossbow),
    none = dss_equipment:lookup(120, crossbow),
    ok = lists:foreach(
        fun(EquipmentID) ->
            crossbow_lookup_test(EquipmentID)
        end,
        lists:seq(116, 119)
    ).


-spec catalyst_lookup_test() -> ok.
catalyst_lookup_test() ->
    none = dss_equipment:lookup(119, catalyst),
    none = dss_equipment:lookup(131, catalyst),
    ok = lists:foreach(
        fun(EquipmentID) ->
            catalyst_lookup_test(EquipmentID)
        end,
        lists:seq(120, 130)
    ).


-spec pyromancy_flame_lookup_test() -> ok.
pyromancy_flame_lookup_test() ->
    none = dss_equipment:lookup(130, catalyst),
    none = dss_equipment:lookup(133, catalyst),
    ok = lists:foreach(
        fun(EquipmentID) ->
            pyromancy_flame_lookup_test(EquipmentID)
        end,
        lists:seq(131, 132)
    ).


-spec talisman_lookup_test() -> ok.
talisman_lookup_test() ->
    none = dss_equipment:lookup(132, talisman),
    none = dss_equipment:lookup(140, talisman),
    ok = lists:foreach(
        fun(EquipmentID) ->
            talisman_lookup_test(EquipmentID)
        end,
        lists:seq(133, 139)
    ).


-spec small_shield_lookup_test() -> ok.
small_shield_lookup_test() ->
    none = dss_equipment:lookup(139, small_shield),
    none = dss_equipment:lookup(151, small_shield),
    ok = lists:foreach(
        fun(EquipmentID) ->
            small_shield_lookup_test(EquipmentID)
        end,
        lists:seq(140, 150)
    ).


-spec normal_shield_lookup_test() -> ok.
normal_shield_lookup_test() ->
    none = dss_equipment:lookup(150, normal_shield),
    none = dss_equipment:lookup(174, normal_shield),
    ok = lists:foreach(
        fun(EquipmentID) ->
            normal_shield_lookup_test(EquipmentID)
        end,
        lists:seq(151, 173)
    ).


-spec large_shield_lookup_test() -> ok.
large_shield_lookup_test() ->
    none = dss_equipment:lookup(173, large_shield),
    none = dss_equipment:lookup(183, large_shield),
    ok = lists:foreach(
        fun(EquipmentID) ->
            large_shield_lookup_test(EquipmentID)
        end,
        lists:seq(174, 182)
    ).


-spec head_armor_lookup_test() -> ok.
head_armor_lookup_test() ->
    none = dss_equipment:lookup(182, head_armor),
    none = dss_equipment:lookup(251, head_armor),
    ok = lists:foreach(
        fun(EquipmentID) ->
            head_armor_lookup_test(EquipmentID)
        end,
        lists:seq(184, 250)
    ).


-spec chest_armor_lookup_test() -> ok.
chest_armor_lookup_test() ->
    none = dss_equipment:lookup(250, chest_armor),
    none = dss_equipment:lookup(307, chest_armor),
    ok = lists:foreach(
        fun(EquipmentID) ->
            chest_armor_lookup_test(EquipmentID)
        end,
        lists:seq(251, 306)
    ).


-spec hand_armor_lookup_test() -> ok.
hand_armor_lookup_test() ->
    none = dss_equipment:lookup(306, hand_armor),
    none = dss_equipment:lookup(360, hand_armor),
    ok = lists:foreach(
        fun(EquipmentID) ->
            hand_armor_lookup_test(EquipmentID)
        end,
        lists:seq(307, 359)
    ).


-spec leg_armor_lookup_test() -> ok.
leg_armor_lookup_test() ->
    none = dss_equipment:lookup(359, leg_armor),
    none = dss_equipment:lookup(411, leg_armor),
    ok = lists:foreach(
        fun(EquipmentID) ->
            leg_armor_lookup_test(EquipmentID)
        end,
        lists:seq(360, 410)
    ).


-spec weapon_lookup_test(
        dss_equipment:id(),
        dss_equipment:equipment_type(),
        unicode:unicode_binary(),
        unicode:unicode_binary(),
        float(),
        dss_equipment:requirement()
    ) -> pos_integer().
weapon_lookup_test(EquipmentID, EqpType, Ename, Jname, Weight, #{strenght := Strength, dexterity := Dexterity, intelligence := Intelligence, faith := Faith}) ->
    ct:pal("EquipmentID: ~p~n", [EquipmentID]),
    {value, Equipment} = dss_equipment:lookup(EqpType, EquipmentID),
    EquipmentID  = maps:get(id, Equipment),
    Ename        = maps:get(english, maps:get(name, Equipment)),
    Jname        = maps:get(japanese, maps:get(name, Equipment)),
    Weight       = maps:get(weight, Equipment),
    Strength     = maps:get(strength, maps:get(requirements, Equipment)),
    Dexterity    = maps:get(dexterity, maps:get(requirements, Equipment)),
    Intelligence = maps:get(intelligence, maps:get(requirements, Equipment)),
    Faith        = maps:get(faith, maps:get(requirements, Equipment)).


-spec dagger_list_test() -> ok.
dagger_list_test() ->
    DaggerList = dss_equipment:list(dagger),
    6 = length(DaggerList),
    ok.


-spec straight_sword_list_test() -> ok.
straight_sword_list_test() ->
    StraightSwordList = dss_equipment:list(straight_sword),
    13 = length(StraightSwordList),
    ok.


-spec greats_sword_list_test() -> ok.
greats_sword_list_test() ->
    GreatsSwordList = dss_equipment:list(greats_sword),
    13 = length(GreatsSwordList),
    ok.


-spec ultra_greatsword_list_test() -> ok.
ultra_greatsword_list_test() ->
    UltraGreatswordList = dss_equipment:list(ultra_greatsword),
    5 = length(UltraGreatswordList),
    ok.


-spec curved_sword_list_test() -> ok.
curved_sword_list_test() ->
    CurvedSwordList = dss_equipment:list(curved_sword),
    7 = length(CurvedSwordList),
    ok.


-spec curved_greatsword_list_test() -> ok.
curved_greatsword_list_test() ->
    CurvedGreatSwordList = dss_equipment:list(curved_greatsword),
    3 = length(CurvedGreatSwordList),
    ok.


-spec thrusting_sword_list_test() -> ok.
thrusting_sword_list_test() ->
    ThrustingSwordList = dss_equipment:list(thrusting_sword),
    5 = length(ThrustingSwordList),
    ok.


-spec katana_list_test() -> ok.
katana_list_test() ->
    KatanaList = dss_equipment:list(katana),
    4 = length(KatanaList),
    ok.


-spec axe_list_test() -> ok.
axe_list_test() ->
    AxeList = dss_equipment:list(axe),
    6 = length(AxeList),
    ok.


-spec greataxe_list_test() -> ok.
greataxe_list_test() ->
    GreatAxeList = dss_equipment:list(greataxe),
    5 = length(GreatAxeList),
    ok.


-spec hammer_list_test() -> ok.
hammer_list_test() ->
    HammerList = dss_equipment:list(hammer),
    9 = length(HammerList),
    ok.


-spec great_hammer_list_test() -> ok.
great_hammer_list_test() ->
    GreatHammerList = dss_equipment:list(great_hammer),
    6 = length(GreatHammerList),
    ok.


-spec spear_list_test() -> ok.
spear_list_test() ->
    SpearList = dss_equipment:list(spear),
    9 = length(SpearList),
    ok.


-spec long_spear_list_test() -> ok.
long_spear_list_test() ->
    LongSpearList = dss_equipment:list(long_spear),
    1 = length(LongSpearList),
    ok.


-spec halberd_list_test() -> ok.
halberd_list_test() ->
    HalberdList = dss_equipment:list(halberd),
    9 = length(HalberdList),
    ok.


-spec whip_list_test() -> ok.
whip_list_test() ->
    WhipList = dss_equipment:list(whip),
    3 = length(WhipList),
    ok.


-spec fist_list_test() -> ok.
fist_list_test() ->
    FistList = dss_equipment:list(fist),
    4 = length(FistList),
    ok.


-spec bow_list_test() -> ok.
bow_list_test() ->
    BowList = dss_equipment:list(bow),
    5 = length(BowList),
    ok.


-spec greatbow_list_test() -> ok.
greatbow_list_test() ->
    GreatBowList = dss_equipment:list(greatbow),
    2 = length(GreatBowList),
    ok.


-spec crossbow_list_test() -> ok.
crossbow_list_test() ->
    CrossbowList = dss_equipment:list(crossbow),
    4 = length(CrossbowList),
    ok.


-spec catalyst_list_test() -> ok.
catalyst_list_test() ->
    CatalystList = dss_equipment:list(catalyst),
    11 = length(CatalystList),
    ok.


-spec pyromancy_flame_list_test() -> ok.
pyromancy_flame_list_test() ->
    PyromancyFlameList = dss_equipment:list(pyromancy_flame),
    2 = length(PyromancyFlameList),
    ok.


-spec talisman_list_test() -> ok.
talisman_list_test() ->
    TalismanList = dss_equipment:list(talisman),
    7 = length(TalismanList),
    ok.


-spec small_shield_list_test() -> ok.
small_shield_list_test() ->
    SmallShieldList = dss_equipment:list(small_shield),
    11 = length(SmallShieldList),
    ok.


-spec normal_shield_list_test() -> ok.
normal_shield_list_test() ->
    NormalShieldList = dss_equipment:list(normal_shield),
    23 = length(NormalShieldList),
    ok.


-spec large_shield_list_test() -> ok.
large_shield_list_test() ->
    LargeShieldList = dss_equipment:list(large_shield),
    9 = length(LargeShieldList),
    ok.


-spec head_armor_list_test() -> ok.
head_armor_list_test() ->
    HeadArmorList = dss_equipment:list(head_armor),
    67 = length(HeadArmorList),
    ok.


-spec chest_armor_list_test() -> ok.
chest_armor_list_test() ->
    ChestArmorList = dss_equipment:list(chest_armor),
    56 = length(ChestArmorList),
    ok.


-spec hand_armor_list_test() -> ok.
hand_armor_list_test() ->
    HandArmorList = dss_equipment:list(hand_armor),
    53 = length(HandArmorList),
    ok.


-spec leg_armor_list_test() -> ok.
leg_armor_list_test() ->
    LegArmorList = dss_equipment:list(leg_armor),
    56 = length(LegArmorList),
    ok.


-spec dagger_lookup_test(pos_integer()) -> pos_integer().
dagger_lookup_test(1) ->
    Requirements = #{ strenght     => 5
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(1, dagger, <<"dagger">>, <<"ダガー"/utf8>>, 0.5, Requirements);
dagger_lookup_test(2) ->
    Requirements = #{ strenght     => 5
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(2, dagger, <<"parrying dagger">>, <<"パリングダガー"/utf8>>, 0.5, Requirements);
dagger_lookup_test(3) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(3, dagger, <<"bandit's knife">>, <<"盗賊の短刀"/utf8>>, 1, Requirements);
dagger_lookup_test(4) ->
    Requirements = #{ strenght     => 5
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(4, dagger, <<"ghost blade">>, <<"亡霊の刃"/utf8>>, 0.5, Requirements);
dagger_lookup_test(5) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 25
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(5, dagger, <<"dark silver tracer">>, <<"暗銀の残滅"/utf8>>, 1, Requirements);
dagger_lookup_test(6) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 20
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(6, dagger, <<"priscilla's dagger">>, <<"プリシラの短剣"/utf8>>, 1, Requirements).


-spec straight_sword_lookup_test(pos_integer()) -> pos_integer().
straight_sword_lookup_test(7) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(7, straight_sword, <<"short sword">>, <<"ショートソード"/utf8>>, 2, Requirements);
straight_sword_lookup_test(8) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(8, straight_sword, <<"long sword">>, <<"ロングソード"/utf8>>, 3, Requirements);
straight_sword_lookup_test(9) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(9, straight_sword, <<"broad sword">>, <<"ブロードソード"/utf8>>, 3, Requirements);
straight_sword_lookup_test(10) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(10, straight_sword, <<"balder side sword">>, <<"バルデルの刺突直剣"/utf8>>, 3, Requirements);
straight_sword_lookup_test(11) ->
    Requirements = #{ strenght     => 12
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(11, straight_sword, <<"sunlight straight sword">>, <<"太陽の直剣"/utf8>>, 4, Requirements);
straight_sword_lookup_test(12) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 16
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(12, straight_sword, <<"dark sword">>, <<"ダークソード"/utf8>>, 6, Requirements);
straight_sword_lookup_test(13) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(13, straight_sword, <<"barbed straight sword">>, <<"トゲの直剣"/utf8>>, 3, Requirements);
straight_sword_lookup_test(14) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(14, straight_sword, <<"crystal straight sword">>, <<"結晶直剣"/utf8>>, 6, Requirements);
straight_sword_lookup_test(15) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 22
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(15, straight_sword, <<"silver knight straight sword">>, <<"銀騎士の剣"/utf8>>, 6, Requirements);
straight_sword_lookup_test(16) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 14},
    weapon_lookup_test(16, straight_sword, <<"astora's straight sword">>, <<"アストラの直剣"/utf8>>, 3, Requirements);
straight_sword_lookup_test(17) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(17, straight_sword, <<"drake sword">>, <<"飛竜の剣"/utf8>>, 6, Requirements);
straight_sword_lookup_test(18) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(18, straight_sword, <<"broken straight sword">>, <<"折れた直剣"/utf8>>, 2, Requirements);
straight_sword_lookup_test(19) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 6
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(19, straight_sword, <<"straight sword hilt">>, <<"直剣の柄"/utf8>>, 1, Requirements).


-spec greats_sword_lookup_test(pos_integer()) -> pos_integer().
greats_sword_lookup_test(20) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(20, greats_sword, <<"bastard sword">>, <<"バスタードソード"/utf8>>, 6, Requirements);
greats_sword_lookup_test(21) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(21, greats_sword, <<"claymore">>, <<"クレイモア"/utf8>>, 6, Requirements);
greats_sword_lookup_test(22) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(22, greats_sword, <<"man serpent greatsword">>, <<"蛇人の大剣"/utf8>>, 10, Requirements);
greats_sword_lookup_test(23) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(23, greats_sword, <<"flamberge">>, <<"フランベルジェ"/utf8>>, 6, Requirements);
greats_sword_lookup_test(24) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(24, greats_sword, <<"crystal greatsword">>, <<"結晶大剣"/utf8>>, 8, Requirements);
greats_sword_lookup_test(25) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 18
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(25, greats_sword, <<"black knight sword">>, <<"黒騎士の剣"/utf8>>, 8, Requirements);
greats_sword_lookup_test(26) ->
    Requirements = #{ strenght     => 40
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(26, greats_sword, <<"stone greatsword">>, <<"石の大剣"/utf8>>, 18, Requirements);
greats_sword_lookup_test(27) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 18
                    , intelligence => 18
                    , faith        => 18},
    weapon_lookup_test(27, greats_sword, <<"greatsword of artorias">>, <<"アルトリウスの大剣"/utf8>>, 10, Requirements);
greats_sword_lookup_test(28) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 18
                    , intelligence => 20
                    , faith        => 20},
    weapon_lookup_test(28, greats_sword, <<"greatsword of artorias(cursed)">>, <<"アルトリウスの大剣（聖）"/utf8>>, 10, Requirements);
greats_sword_lookup_test(29) ->
    Requirements = #{ strenght     => 22
                    , dexterity    => 18
                    , intelligence => 18
                    , faith        => 18},
    weapon_lookup_test(29, greats_sword, <<"abyss greatsword">>, <<"深淵の大剣"/utf8>>, 9, Requirements);
greats_sword_lookup_test(30) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(30, greats_sword, <<"great lord greatsword">>, <<"大王の大剣"/utf8>>, 8, Requirements);
greats_sword_lookup_test(31) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 10
                    , intelligence => 28
                    , faith        => 0},
    weapon_lookup_test(31, greats_sword, <<"moonlight greatsword">>, <<"月光の大剣"/utf8>>, 6, Requirements);
greats_sword_lookup_test(32) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 16
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(32, greats_sword, <<"obsidian greatsword">>, <<"黒竜の大剣"/utf8>>, 8, Requirements).


-spec ultra_greatsword_lookup_test(pos_integer()) -> pos_integer().
ultra_greatsword_lookup_test(33) ->
    Requirements = #{ strenght     => 28
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(33, ultra_greatsword, <<"greatsword">>, <<"グレートソード"/utf8>>, 12, Requirements);
ultra_greatsword_lookup_test(34) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(34, ultra_greatsword, <<"zweihander">>, <<"ツヴァイヘンダー"/utf8>>, 10, Requirements);
ultra_greatsword_lookup_test(35) ->
    Requirements = #{ strenght     => 40
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(35, ultra_greatsword, <<"demon great machete">>, <<"デーモンの大鉈"/utf8>>, 18, Requirements);
ultra_greatsword_lookup_test(36) ->
    Requirements = #{ strenght     => 32
                    , dexterity    => 18
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(36, ultra_greatsword, <<"black knight greatsword">>, <<"黒騎士の大剣"/utf8>>, 14, Requirements);
ultra_greatsword_lookup_test(37) ->
    Requirements = #{ strenght     => 50
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(37, ultra_greatsword, <<"dragon greatsword">>, <<"古竜の大剣"/utf8>>, 24, Requirements).


-spec curved_sword_lookup_test(pos_integer()) -> pos_integer().
curved_sword_lookup_test(38) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(38, curved_sword, <<"scimitar">>, <<"シミター"/utf8>>, 1.5, Requirements);
curved_sword_lookup_test(39) ->
    Requirements = #{ strenght     => 9
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(39, curved_sword, <<"falchion">>, <<"ファルシオン"/utf8>>, 2.5, Requirements);
curved_sword_lookup_test(40) ->
    Requirements = #{ strenght     => 9
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(40, curved_sword, <<"shotel">>, <<"ショーテル"/utf8>>, 2.5, Requirements);
curved_sword_lookup_test(41) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 20
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(41, curved_sword, <<"painting guardian sword">>, <<"絵画守りの曲刀"/utf8>>, 1.5, Requirements);
curved_sword_lookup_test(42) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(42, curved_sword, <<"jagged ghost blade">>, <<"亡霊のギザギザ刃"/utf8>>, 1.5, Requirements);
curved_sword_lookup_test(43) ->
    Requirements = #{ strenght     => 9
                    , dexterity    => 25
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(43, curved_sword, <<"gold tracer">>, <<"黄金の残光"/utf8>>, 2, Requirements);
curved_sword_lookup_test(44) ->
    Requirements = #{ strenght     => 11
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(44, curved_sword, <<"quelaag's fury sword">>, <<"クラーグの魔剣"/utf8>>, 3.5, Requirements).


-spec curved_greatsword_lookup_test(pos_integer()) -> pos_integer().
curved_greatsword_lookup_test(45) ->
    Requirements = #{ strenght     => 28
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(45, curved_greatsword, <<"murakumo">>, <<"ムラクモ"/utf8>>, 12, Requirements);
curved_greatsword_lookup_test(46) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(46, curved_greatsword, <<"server">>, <<"生贄刀"/utf8>>, 10, Requirements);
curved_greatsword_lookup_test(47) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(47, curved_greatsword, <<"gravelord sword">>, <<"墓王の剣"/utf8>>, 10, Requirements).


-spec thrusting_sword_lookup_test(pos_integer()) -> pos_integer().
thrusting_sword_lookup_test(48) ->
    Requirements = #{ strenght     => 5
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(48, thrusting_sword, <<"mail breaker">>, <<"鎧貫き"/utf8>>, 0.5, Requirements);
thrusting_sword_lookup_test(49) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(49, thrusting_sword, <<"rapier">>, <<"レイピア"/utf8>>, 1.5, Requirements);
thrusting_sword_lookup_test(50) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(50, thrusting_sword, <<"estoc">>, <<"エストック"/utf8>>, 3, Requirements);
thrusting_sword_lookup_test(51) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 20
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(51, thrusting_sword, <<"ricard's rapier">>, <<"リカールの刺剣"/utf8>>, 2, Requirements);
thrusting_sword_lookup_test(52) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 16
                    , intelligence => 16
                    , faith        => 0},
    weapon_lookup_test(52, thrusting_sword, <<"velka's rapier">>, <<"ベルカの刺剣"/utf8>>, 2, Requirements).


-spec katana_lookup_test(pos_integer()) -> pos_integer().
katana_lookup_test(53) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(53, katana, <<"uchigatana">>, <<"打刀"/utf8>>, 5, Requirements);
katana_lookup_test(54) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 20
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(54, katana, <<"iaito">>, <<"居合い刀"/utf8>>, 5, Requirements);
katana_lookup_test(55) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 16
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(55, katana, <<"washing pole">>, <<"物干し竿"/utf8>>, 8, Requirements);
katana_lookup_test(56) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(56, katana, <<"chaos blade">>, <<"混沌の刃"/utf8>>, 6, Requirements).


-spec axe_lookup_test(pos_integer()) -> pos_integer().
axe_lookup_test(57) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(57, axe, <<"hand axe">>, <<"ハンドアクス"/utf8>>, 2, Requirements);
axe_lookup_test(58) ->
    Requirements = #{ strenght     => 12
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(58, axe, <<"battle axe">>, <<"バトルアクス"/utf8>>, 4, Requirements);
axe_lookup_test(59) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(59, axe, <<"butcher knife">>, <<"肉断ち包丁"/utf8>>, 10, Requirements);
axe_lookup_test(60) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(60, axe, <<"gargoyle tail axe">>, <<"ガーゴイルの尾斧"/utf8>>, 5, Requirements);
axe_lookup_test(61) ->
    Requirements = #{ strenght     => 18
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 16},
    weapon_lookup_test(61, axe, <<"crescent axe">>, <<"三日月斧"/utf8>>, 7, Requirements);
axe_lookup_test(62) ->
    Requirements = #{ strenght     => 36
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(62, axe, <<"golem axe">>, <<"ゴーレムアクス"/utf8>>, 16, Requirements).


-spec greataxe_lookup_test(pos_integer()) -> pos_integer().
greataxe_lookup_test(63) ->
    Requirements = #{ strenght     => 32
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(63, greataxe, <<"greataxe">>, <<"グレートアクス"/utf8>>, 14, Requirements);
greataxe_lookup_test(64) ->
    Requirements = #{ strenght     => 46
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(64, greataxe, <<"demon's greataxe">>, <<"デーモンの大斧"/utf8>>, 22, Requirements);
greataxe_lookup_test(65) ->
    Requirements = #{ strenght     => 48
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(65, greataxe, <<"stone greataxe">>, <<"岩の大斧"/utf8>>, 24, Requirements);
greataxe_lookup_test(66) ->
    Requirements = #{ strenght     => 36
                    , dexterity    => 18
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(66, greataxe, <<"black knight greataxe">>, <<"黒騎士の大斧"/utf8>>, 16, Requirements);
greataxe_lookup_test(67) ->
    Requirements = #{ strenght     => 50
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(67, greataxe, <<"dragon king greataxe">>, <<"竜王の大斧"/utf8>>, 24, Requirements).


-spec hammer_lookup_test(pos_integer()) -> pos_integer().
hammer_lookup_test(68) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(68, hammer, <<"club">>, <<"クラブ"/utf8>>, 3, Requirements);
hammer_lookup_test(69) ->
    Requirements = #{ strenght     => 12
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(69, hammer, <<"reinforced club">>, <<"強化クラブ"/utf8>>, 4, Requirements);
hammer_lookup_test(70) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(70, hammer, <<"blacksmith giant hammer">>, <<"巨人鍛冶の木槌"/utf8>>, 6, Requirements);
hammer_lookup_test(71) ->
    Requirements = #{ strenght     => 12
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(71, hammer, <<"mace">>, <<"メイス"/utf8>>, 4, Requirements);
hammer_lookup_test(72) ->
    Requirements = #{ strenght     => 11
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(72, hammer, <<"morning star">>, <<"モーニングスター"/utf8>>, 4, Requirements);
hammer_lookup_test(73) ->
    Requirements = #{ strenght     => 11
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(73, hammer, <<"warpick">>, <<"ウォーピック"/utf8>>, 3.5, Requirements);
hammer_lookup_test(74) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(74, hammer, <<"pickaxe">>, <<"つるはし"/utf8>>, 5, Requirements);
hammer_lookup_test(75) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(75, hammer, <<"blacksmith hammer">>, <<"鍛冶屋の金槌"/utf8>>, 5, Requirements);
hammer_lookup_test(76) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(76, hammer, <<"hammer of vamos">>, <<"バモスのハンマー"/utf8>>, 5, Requirements).


-spec great_hammer_lookup_test(pos_integer()) -> pos_integer().
great_hammer_lookup_test(77) ->
    Requirements = #{ strenght     => 26
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(77, great_hammer, <<"large club">>, <<"ラージクラブ"/utf8>>, 11, Requirements);
great_hammer_lookup_test(78) ->
    Requirements = #{ strenght     => 28
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(78, great_hammer, <<"great club">>, <<"グレートクラブ"/utf8>>, 12, Requirements);
great_hammer_lookup_test(79) ->
    Requirements = #{ strenght     => 46
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(79, great_hammer, <<"demon's great hammer">>, <<"デーモンの大槌"/utf8>>, 22, Requirements);
great_hammer_lookup_test(80) ->
    Requirements = #{ strenght     => 50
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 30},
    weapon_lookup_test(80, great_hammer, <<"grant">>, <<"グラント"/utf8>>, 24, Requirements);
great_hammer_lookup_test(81) ->
    Requirements = #{ strenght     => 40
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(81, great_hammer, <<"dragon tooth">>, <<"大竜牙"/utf8>>, 18, Requirements);
great_hammer_lookup_test(82) ->
    Requirements = #{ strenght     => 58
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(82, great_hammer, <<"smough's hammer">>, <<"スモウハンマー"/utf8>>, 28, Requirements).


-spec spear_lookup_test(pos_integer()) -> pos_integer().
spear_lookup_test(83) ->
    Requirements = #{ strenght     => 11
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(83, spear, <<"spear">>, <<"スピア"/utf8>>, 3.5, Requirements);
spear_lookup_test(84) ->
    Requirements = #{ strenght     => 13
                    , dexterity    => 15
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(84, spear, <<"winged spear">>, <<"ウィングドスピア"/utf8>>, 4.5, Requirements);
spear_lookup_test(85) ->
    Requirements = #{ strenght     => 13
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(85, spear, <<"partizan">>, <<"パルチザン"/utf8>>, 4.5, Requirements);
spear_lookup_test(86) ->
    Requirements = #{ strenght     => 15
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(86, spear, <<"four-pronged plow">>, <<"四又鋤"/utf8>>, 5.5, Requirements);
spear_lookup_test(87) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 16
                    , intelligence => 24
                    , faith        => 0},
    weapon_lookup_test(87, spear, <<"channeler's trident">>, <<"伝道者の三又槍"/utf8>>, 6, Requirements);
spear_lookup_test(88) ->
    Requirements = #{ strenght     => 12
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(88, spear, <<"demon's spear">>, <<"デーモンの槍"/utf8>>, 4, Requirements);
spear_lookup_test(89) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 22
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(89, spear, <<"silver knight spear">>, <<"銀騎士の槍"/utf8>>, 6, Requirements);
spear_lookup_test(90) ->
    Requirements = #{ strenght     => 12
                    , dexterity    => 0
                    , intelligence => 14
                    , faith        => 0},
    weapon_lookup_test(90, spear, <<"moonlight butterfly horn">>, <<"月光蝶の角"/utf8>>, 4, Requirements);
spear_lookup_test(91) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 24
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(91, spear, <<"dragonslayer spear">>, <<"竜狩りの槍"/utf8>>, 10, Requirements).


-spec long_spear_lookup_test(pos_integer()) -> pos_integer().
long_spear_lookup_test(92) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(92, long_spear, <<"pike">>, <<"パイク"/utf8>>, 10, Requirements).


-spec halberd_lookup_test(pos_integer()) -> pos_integer().
halberd_lookup_test(93) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(93, halberd, <<"halberd">>, <<"ハルバード"/utf8>>, 6, Requirements);
halberd_lookup_test(94) ->
    Requirements = #{ strenght     => 15
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(94, halberd, <<"lucerne">>, <<"ルッツエルン"/utf8>>, 5.5, Requirements);
halberd_lookup_test(95) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(95, halberd, <<"scythe">>, <<"サイズ"/utf8>>, 5, Requirements);
halberd_lookup_test(96) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(96, halberd, <<"gargoyle's halberd">>, <<"ガーゴイルの斧槍"/utf8>>, 6, Requirements);
halberd_lookup_test(97) ->
    Requirements = #{ strenght     => 36
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(97, halberd, <<"giant's halberd">>, <<"巨人のハルバード"/utf8>>, 16, Requirements);
halberd_lookup_test(98) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(98, halberd, <<"titanite catch pole">>, <<"くさびの刺又"/utf8>>, 6, Requirements);
halberd_lookup_test(99) ->
    Requirements = #{ strenght     => 32
                    , dexterity    => 18
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(99, halberd, <<"black knight halberd">>, <<"黒騎士の斧槍"/utf8>>, 14, Requirements);
halberd_lookup_test(100) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(100, halberd, <<"great scythe">>, <<"大鎌"/utf8>>, 5, Requirements);
halberd_lookup_test(101) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(101, halberd, <<"lifehunt scythe">>, <<"生命狩りの鎌"/utf8>>, 6, Requirements).


-spec whip_lookup_test(pos_integer()) -> pos_integer().
whip_lookup_test(102) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(102, whip, <<"whip">>, <<"ウィップ"/utf8>>, 1.5, Requirements);
whip_lookup_test(103) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 16
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(103, whip, <<"notched whip">>, <<"イバラムチ"/utf8>>, 2, Requirements);
whip_lookup_test(104) ->
    Requirements = #{ strenght     => 15
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(104, whip, <<"guardian tail">>, <<"聖獣の尾"/utf8>>, 5, Requirements).


-spec fist_lookup_test(pos_integer()) -> pos_integer().
fist_lookup_test(105) ->
    Requirements = #{ strenght     => 5
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(105, fist, <<"caestus">>, <<"セスタス"/utf8>>, 0.5, Requirements);
fist_lookup_test(106) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(106, fist, <<"craw">>, <<"かぎ爪"/utf8>>, 1, Requirements);
fist_lookup_test(107) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(107, fist, <<"dragon bone fist">>, <<"竜骨の拳"/utf8>>, 8, Requirements);
fist_lookup_test(108) ->
    Requirements = #{ strenght     => 0
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(108, fist, <<"dark hand">>, <<"ダークハンド"/utf8>>, 0.5, Requirements).


-spec bow_lookup_test(pos_integer()) -> pos_integer().
bow_lookup_test(109) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(109, bow, <<"short bow">>, <<"ショートボウ"/utf8>>, 0.5, Requirements);
bow_lookup_test(110) ->
    Requirements = #{ strenght     => 11
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(110, bow, <<"composite bow">>, <<"コンポジットボウ"/utf8>>, 1, Requirements);
bow_lookup_test(111) ->
    Requirements = #{ strenght     => 9
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(111, bow, <<"long bow">>, <<"ロングボウ"/utf8>>, 1, Requirements);
bow_lookup_test(112) ->
    Requirements = #{ strenght     => 9
                    , dexterity    => 18
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(112, bow, <<"black bow of pharis">>, <<"ファリスの黒弓"/utf8>>, 1, Requirements);
bow_lookup_test(113) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 16
                    , intelligence => 0
                    , faith        => 16},
    weapon_lookup_test(113, bow, <<"darkmoon bow">>, <<"暗月の弓"/utf8>>, 1, Requirements).


-spec greatbow_lookup_test(pos_integer()) -> pos_integer().
greatbow_lookup_test(114) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 20
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(114, greatbow, <<"dragonslayer greatbow">>, <<"竜狩りの大弓"/utf8>>, 10, Requirements);
greatbow_lookup_test(115) ->
    Requirements = #{ strenght     => 27
                    , dexterity    => 20
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(115, greatbow, <<"gough's greatbow">>, <<"ゴーの大弓"/utf8>>, 13, Requirements).


-spec crossbow_lookup_test(pos_integer()) -> pos_integer().
crossbow_lookup_test(116) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(116, crossbow, <<"light crossbow">>, <<"ライトクロスボウ"/utf8>>, 3, Requirements);
crossbow_lookup_test(117) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(117, crossbow, <<"heavy crossbow">>, <<"ヘビークロスボウ"/utf8>>, 5, Requirements);
crossbow_lookup_test(118) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 16
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(118, crossbow, <<"sniper crossbow">>, <<"スナイパークロス"/utf8>>, 8, Requirements);
crossbow_lookup_test(119) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(119, crossbow, <<"avelyn">>, <<"アヴェリン"/utf8>>, 6, Requirements).


-spec catalyst_lookup_test(pos_integer()) -> pos_integer().
catalyst_lookup_test(120) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 0
                    , intelligence => 10
                    , faith        => 0},
    weapon_lookup_test(120, catalyst, <<"sorcerer's catalyst">>, <<"魔術師の杖"/utf8>>, 2, Requirements);
catalyst_lookup_test(121) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 0
                    , intelligence => 10
                    , faith        => 0},
    weapon_lookup_test(121, catalyst, <<"beatrice's catalyst">>, <<"ビアトリスの杖"/utf8>>, 2, Requirements);
catalyst_lookup_test(122) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 0
                    , intelligence => 24
                    , faith        => 0},
    weapon_lookup_test(122, catalyst, <<"logan's catalyst">>, <<"ローガンの杖"/utf8>>, 2, Requirements);
catalyst_lookup_test(123) ->
    Requirements = #{ strenght     => 3
                    , dexterity    => 0
                    , intelligence => 12
                    , faith        => 0},
    weapon_lookup_test(123, catalyst, <<"oolacile ivory catalyst">>, <<"ウーラシールの白枝"/utf8>>, 0.5, Requirements);
catalyst_lookup_test(124) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 0
                    , intelligence => 10
                    , faith        => 0},
    weapon_lookup_test(124, catalyst, <<"oolacile catalyst">>, <<"ウーラシールの枝杖"/utf8>>, 2, Requirements);
catalyst_lookup_test(125) ->
    Requirements = #{ strenght     => 12
                    , dexterity    => 10
                    , intelligence => 10
                    , faith        => 0},
    weapon_lookup_test(125, catalyst, <<"demon's catalyst">>, <<"デーモンの杖"/utf8>>, 4, Requirements);
catalyst_lookup_test(126) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 0
                    , intelligence => 14
                    , faith        => 0},
    weapon_lookup_test(126, catalyst, <<"izalith catalyst">>, <<"イザリスの杖"/utf8>>, 2, Requirements);
catalyst_lookup_test(127) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 10
                    , intelligence => 12
                    , faith        => 0},
    weapon_lookup_test(127, catalyst, <<"tin banishment catalyst">>, <<"封印の錫杖"/utf8>>, 3, Requirements);
catalyst_lookup_test(128) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 0
                    , intelligence => 32
                    , faith        => 0},
    weapon_lookup_test(128, catalyst, <<"tin crystallization catalyst">>, <<"結晶の錫杖"/utf8>>, 2.5, Requirements);
catalyst_lookup_test(129) ->
    Requirements = #{ strenght     => 4
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 16},
    weapon_lookup_test(129, catalyst, <<"tin darkmoon catalyst">>, <<"暗月の錫杖"/utf8>>, 1, Requirements);
catalyst_lookup_test(130) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 0
                    , intelligence => 13
                    , faith        => 0},
    weapon_lookup_test(130, catalyst, <<"manus catalyst">>, <<"マヌスの大杖"/utf8>>, 5, Requirements).


-spec pyromancy_flame_lookup_test(pos_integer()) -> pos_integer().
pyromancy_flame_lookup_test(131) ->
    Requirements = #{ strenght     => 4
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(131, pyromancy_flame, <<"pyromancy flame">>, <<"呪術の火"/utf8>>, 0, Requirements);
pyromancy_flame_lookup_test(132) ->
    Requirements = #{ strenght     => 4
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(132, pyromancy_flame, <<"ascended pyromancy flame">>, <<"呪術の火（進化後）"/utf8>>, 0, Requirements).


-spec talisman_lookup_test(pos_integer()) -> pos_integer().
talisman_lookup_test(133) ->
    Requirements = #{ strenght     => 4
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 10},
    weapon_lookup_test(133, talisman, <<"talisman">>, <<"タリスマン"/utf8>>, 0.3, Requirements);
talisman_lookup_test(134) ->
    Requirements = #{ strenght     => 4
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 14},
    weapon_lookup_test(134, talisman, <<"canvas talisman">>, <<"粗布のタリスマン"/utf8>>, 0.3, Requirements);
talisman_lookup_test(135) ->
    Requirements = #{ strenght     => 4
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 10},
    weapon_lookup_test(135, talisman, <<"thorolund talisman">>, <<"ソルロンドのタリスマン"/utf8>>, 0.3, Requirements);
talisman_lookup_test(136) ->
    Requirements = #{ strenght     => 4
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 16},
    weapon_lookup_test(136, talisman, <<"ivory talisman">>, <<"白のタリスマン"/utf8>>, 0.3, Requirements);
talisman_lookup_test(137) ->
    Requirements = #{ strenght     => 4
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 14},
    weapon_lookup_test(137, talisman, <<"sunlight talisman">>, <<"太陽のタリスマン"/utf8>>, 0.3, Requirements);
talisman_lookup_test(138) ->
    Requirements = #{ strenght     => 4
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 24},
    weapon_lookup_test(138, talisman, <<"darkmoon talisman">>, <<"暗月のタリスマン"/utf8>>, 0.3, Requirements);
talisman_lookup_test(139) ->
    Requirements = #{ strenght     => 4
                    , dexterity    => 0
                    , intelligence => 16
                    , faith        => 0},
    weapon_lookup_test(139, talisman, <<"velka's talisman">>, <<"ベルカのタリスマン"/utf8>>, 0.3, Requirements).


-spec small_shield_lookup_test(pos_integer()) -> pos_integer().
small_shield_lookup_test(140) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(140, small_shield, <<"warrior's round shield">>, <<"戦士の円盾"/utf8>>, 1, Requirements);
small_shield_lookup_test(141) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(141, small_shield, <<"red and white round shield">>, <<"紅白の円盾"/utf8>>, 1, Requirements);
small_shield_lookup_test(142) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(142, small_shield, <<"caduceus round shield">>, <<"双蛇の円盾"/utf8>>, 1, Requirements);
small_shield_lookup_test(143) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 16},
    weapon_lookup_test(143, small_shield, <<"effigy shield">>, <<"邪神の盾"/utf8>>, 3, Requirements);
small_shield_lookup_test(144) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(144, small_shield, <<"cracked round shield">>, <<"壊れかけの木盾"/utf8>>, 1, Requirements);
small_shield_lookup_test(145) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(145, small_shield, <<"plank shield">>, <<"木板の盾"/utf8>>, 1.5, Requirements);
small_shield_lookup_test(146) ->
    Requirements = #{ strenght     => 5
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(146, small_shield, <<"small leather shield">>, <<"スモールレザーシールド"/utf8>>, 0.5, Requirements);
small_shield_lookup_test(147) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(147, small_shield, <<"leather shield">>, <<"レザーシールド"/utf8>>, 1, Requirements);
small_shield_lookup_test(148) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(148, small_shield, <<"buckler">>, <<"バックラー"/utf8>>, 1.5, Requirements);
small_shield_lookup_test(149) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 11
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(149, small_shield, <<"target shield">>, <<"ターゲットシールド"/utf8>>, 2, Requirements);
small_shield_lookup_test(150) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(150, small_shield, <<"crystal ring shield">>, <<"結晶輪の盾"/utf8>>, 3, Requirements).


-spec normal_shield_lookup_test(pos_integer()) -> pos_integer().
normal_shield_lookup_test(151) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(151, normal_shield, <<"east-west shield">>, <<"双鳥の木盾"/utf8>>, 1, Requirements);
normal_shield_lookup_test(152) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(152, normal_shield, <<"wooden shield">>, <<"ウッドシールド"/utf8>>, 1.5, Requirements);
normal_shield_lookup_test(153) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(153, normal_shield, <<"large leather shield">>, <<"ラージレザーシールド"/utf8>>, 1.5, Requirements);
normal_shield_lookup_test(154) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(154, normal_shield, <<"heater shield">>, <<"ヒーターシールド"/utf8>>, 2, Requirements);
normal_shield_lookup_test(155) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(155, normal_shield, <<"tower kite shield">>, <<"塔のカイトシールド"/utf8>>, 3, Requirements);
normal_shield_lookup_test(156) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(156, normal_shield, <<"caduceus kite shield">>, <<"双蛇のカイトシールド"/utf8>>, 3, Requirements);
normal_shield_lookup_test(157) ->
    Requirements = #{ strenght     => 11
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(157, normal_shield, <<"hollow soldier shield">>, <<"亡者兵士の盾"/utf8>>, 3.5, Requirements);
normal_shield_lookup_test(158) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(158, normal_shield, <<"knight shield">>, <<"ナイトシールド"/utf8>>, 5.5, Requirements);
normal_shield_lookup_test(159) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 18},
    weapon_lookup_test(159, normal_shield, <<"sanctus">>, <<"サンクトゥス"/utf8>>, 3, Requirements);
normal_shield_lookup_test(160) ->
    Requirements = #{ strenght     => 12
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(160, normal_shield, <<"balder shield">>, <<"バルデルの盾"/utf8>>, 4, Requirements);
normal_shield_lookup_test(161) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(161, normal_shield, <<"spider shield">>, <<"蜘蛛の盾"/utf8>>, 3, Requirements);normal_shield_lookup_test(162) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(162, normal_shield, <<"grass crest shield">>, <<"草紋の盾"/utf8>>, 3, Requirements);
normal_shield_lookup_test(163) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(163, normal_shield, <<"bloodshield">>, <<"血の盾"/utf8>>, 3, Requirements);
normal_shield_lookup_test(164) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(164, normal_shield, <<"iron round shield">>, <<"鉄の円盾"/utf8>>, 5, Requirements);
normal_shield_lookup_test(165) ->
    Requirements = #{ strenght     => 12
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(165, normal_shield, <<"sunlight shield">>, <<"太陽の盾"/utf8>>, 4, Requirements);
normal_shield_lookup_test(166) ->
    Requirements = #{ strenght     => 11
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(166, normal_shield, <<"pierce shield">>, <<"ピアスシールド"/utf8>>, 3.5, Requirements);
normal_shield_lookup_test(167) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(167, normal_shield, <<"spiked shield">>, <<"トゲの盾"/utf8>>, 3, Requirements);
normal_shield_lookup_test(168) ->
    Requirements = #{ strenght     => 12
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(168, normal_shield, <<"gargoyle's shield">>, <<"ガーゴイルの盾"/utf8>>, 4, Requirements);
normal_shield_lookup_test(169) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(169, normal_shield, <<"crystal shield">>, <<"結晶の盾"/utf8>>, 5, Requirements);
normal_shield_lookup_test(170) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(170, normal_shield, <<"crest shield">>, <<"紋章の盾"/utf8>>, 3, Requirements);
normal_shield_lookup_test(171) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(171, normal_shield, <<"dragon crest shield">>, <<"竜紋章の盾"/utf8>>, 3, Requirements);
normal_shield_lookup_test(172) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(172, normal_shield, <<"silver knight shield">>, <<"銀騎士の盾"/utf8>>, 5, Requirements);
normal_shield_lookup_test(173) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(173, normal_shield, <<"black knight shield">>, <<"黒騎士の盾"/utf8>>, 6, Requirements).


-spec large_shield_lookup_test(pos_integer()) -> pos_integer().
large_shield_lookup_test(174) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(174, large_shield, <<"eagle shield">>, <<"大鷲の盾"/utf8>>, 6, Requirements);
large_shield_lookup_test(175) ->
    Requirements = #{ strenght     => 30
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(175, large_shield, <<"tower shield">>, <<"タワーシールド"/utf8>>, 13, Requirements);
large_shield_lookup_test(176) ->
    Requirements = #{ strenght     => 34
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(176, large_shield, <<"black iron greatshield">>, <<"黒鉄の大盾"/utf8>>, 16, Requirements);
large_shield_lookup_test(177) ->
    Requirements = #{ strenght     => 36
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(177, large_shield, <<"giant shield">>, <<"巨人の盾"/utf8>>, 18, Requirements);
large_shield_lookup_test(178) ->
    Requirements = #{ strenght     => 30
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(178, large_shield, <<"bonewheel shield">>, <<"骸骨車輪の盾"/utf8>>, 12, Requirements);
large_shield_lookup_test(179) ->
    Requirements = #{ strenght     => 38
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(179, large_shield, <<"stone greatshield">>, <<"石の大盾"/utf8>>, 20, Requirements);
large_shield_lookup_test(180) ->
    Requirements = #{ strenght     => 50
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(180, large_shield, <<"havel's greatshield">>, <<"ハベルの大盾"/utf8>>, 26, Requirements);
large_shield_lookup_test(181) ->
    Requirements = #{ strenght     => 34
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(181, large_shield, <<"greatshield of artorias">>, <<"アルトリウスの大盾"/utf8>>, 16, Requirements);
large_shield_lookup_test(182) ->
    Requirements = #{ strenght     => 31
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(182, large_shield, <<"cleansing greatshield">>, <<"結晶の大盾"/utf8>>, 14.5, Requirements).


-spec lanthanum_lookup_test() -> pos_integer().
lanthanum_lookup_test() ->
    Requirements = #{ strenght     => 5
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(183, lanthanum, <<"skull lanthanum">>, <<"頭蓋ランタン"/utf8>>, 0.5, Requirements),
    ok.


-spec head_armor_lookup_test(
        dss_equipment:id(),
        unicode:unicode_binary(),
        unicode:unicode_binary(),
        float() | pos_integer(),
        dss_maybe:maybe(pos_integer())
    ) -> dss_maybe:maybe(pos_integer()).
head_armor_lookup_test(EquipmentID, Ename, Jname, Weight, MaybeEWM) ->
    ct:pal("EquipmentID: ~p~n", [EquipmentID]),
    {value, Equipment} = dss_equipment:lookup(head_armor, EquipmentID),
    EquipmentID  = maps:get(id, Equipment),
    Ename        = maps:get(english, maps:get(name, Equipment)),
    Jname        = maps:get(japanese, maps:get(name, Equipment)),
    Weight       = maps:get(weight, Equipment),
    MaybeEWM     = maps:get(equipWeightMagnification, Equipment).


-spec head_armor_lookup_test(pos_integer()) -> dss_maybe:maybe(float()).
head_armor_lookup_test(184) ->
    head_armor_lookup_test(184, <<"standard helm">>, <<"アイアンヘルム"/utf8>>, 3.5, none);
head_armor_lookup_test(185) ->
    head_armor_lookup_test(185, <<"chain helm">>, <<"チェインヘルム"/utf8>>, 3, none);
head_armor_lookup_test(186) ->
    head_armor_lookup_test(186, <<"knight helm">>, <<"騎士の兜"/utf8>>, 4.2, none);
head_armor_lookup_test(187) ->
    head_armor_lookup_test(187, <<"elite knight helm">>, <<"上級騎士の兜"/utf8>>, 4.5, none);
head_armor_lookup_test(188) ->
    head_armor_lookup_test(188, <<"wanderer hood">>, <<"放浪のフード"/utf8>>, 1.4, none);
head_armor_lookup_test(189) ->
    head_armor_lookup_test(189, <<"thief mask">>, <<"盗人マスク"/utf8>>, 1.2, none);
head_armor_lookup_test(190) ->
    head_armor_lookup_test(190, <<"brigand hood">>, <<"山賊の頭巾"/utf8>>, 1.2, none);
head_armor_lookup_test(191) ->
    head_armor_lookup_test(191, <<"sorcerer hat">>, <<"魔術師の帽子"/utf8>>, 0.9, none);
head_armor_lookup_test(192) ->
    head_armor_lookup_test(192, <<"black sorcerer hat">>, <<"魔術師の黒帽子"/utf8>>, 0.7, none);
head_armor_lookup_test(193) ->
    head_armor_lookup_test(193, <<"tattered cloth hood">>, <<"ボロ布のフード"/utf8>>, 1.1, none);
head_armor_lookup_test(194) ->
    head_armor_lookup_test(194, <<"priest`s hat">>, <<"司祭の帽子"/utf8>>, 1.2, none);
head_armor_lookup_test(195) ->
    head_armor_lookup_test(195, <<"cleric helm">>, <<"聖職の兜"/utf8>>, 4.8, none);
head_armor_lookup_test(196) ->
    head_armor_lookup_test(196, <<"iron helm">>, <<"鉄の兜"/utf8>>, 4.5, none);
head_armor_lookup_test(197) ->
    head_armor_lookup_test(197, <<"black iron helm">>, <<"黒鉄の兜"/utf8>>, 6, none);
head_armor_lookup_test(198) ->
    head_armor_lookup_test(198, <<"helm of the wise">>, <<"知恵者の兜"/utf8>>, 5.1, none);
head_armor_lookup_test(199) ->
    head_armor_lookup_test(199, <<"catarina helm">>, <<"カタリナヘルム"/utf8>>, 4.5, none);
head_armor_lookup_test(200) ->
    head_armor_lookup_test(200, <<"crystalline helm">>, <<"結晶付きの兜"/utf8>>, 4.2, none);
head_armor_lookup_test(201) ->
    head_armor_lookup_test(201, <<"brass helm">>, <<"真鍮の兜"/utf8>>, 5.1, none);
head_armor_lookup_test(202) ->
    head_armor_lookup_test(202, <<"pharis`s hat">>, <<"ファリスの帽子"/utf8>>, 1.2, none);
head_armor_lookup_test(203) ->
    head_armor_lookup_test(203, <<"big hat">>, <<"ビッグハット"/utf8>>, 3, none);
head_armor_lookup_test(204) ->
    head_armor_lookup_test(204, <<"mask of the sealer">>, <<"封印者の仮面"/utf8>>, 1.5, none);
head_armor_lookup_test(205) ->
    head_armor_lookup_test(205, <<"witch hat">>, <<"魔女のとんがり帽子"/utf8>>, 1.1, none);
head_armor_lookup_test(206) ->
    head_armor_lookup_test(206, <<"crown of dusk">>, <<"宵闇の頭冠"/utf8>>, 0.4, none);
head_armor_lookup_test(207) ->
    head_armor_lookup_test(207, <<"gold-hemmed black hood">>, <<"黒金糸のフード"/utf8>>, 1.4, none);
head_armor_lookup_test(208) ->
    head_armor_lookup_test(208, <<"mask of velka">>, <<"ベルカの仮面"/utf8>>, 2.5, none);
head_armor_lookup_test(209) ->
    head_armor_lookup_test(209, <<"maiden hood">>, <<"聖女のフード"/utf8>>, 0.8, none);
head_armor_lookup_test(210) ->
    head_armor_lookup_test(210, <<"dingy hood">>, <<"うす汚れたフード"/utf8>>, 0.8, none);
head_armor_lookup_test(211) ->
    head_armor_lookup_test(211, <<"eastern helm">>, <<"東国の兜"/utf8>>, 4.7, none);
head_armor_lookup_test(212) ->
   head_armor_lookup_test(212, <<"shadow mask">>, <<"影の覆面"/utf8>>, 0.9, none);
head_armor_lookup_test(213) ->
   head_armor_lookup_test(213, <<"hollow warrior helm">>, <<"亡者戦士の兜"/utf8>>, 2.6, none);
head_armor_lookup_test(214) ->
   head_armor_lookup_test(214, <<"hollow soldier helm">>, <<"亡者兵士の兜"/utf8>>, 3, none);
head_armor_lookup_test(215) ->
   head_armor_lookup_test(215, <<"balder helm">>, <<"バルデルの兜"/utf8>>, 4.2, none);
head_armor_lookup_test(216) ->
   head_armor_lookup_test(216, <<"steel helm">>, <<"鋼鉄の兜"/utf8>>, 5.4, none);
head_armor_lookup_test(217) ->
   head_armor_lookup_test(217, <<"hollow thief`s hood">>, <<"亡者盗賊のフード"/utf8>>, 1.1, none);
head_armor_lookup_test(218) ->
   head_armor_lookup_test(218, <<"silver knight helm">>, <<"銀騎士の兜"/utf8>>, 4.8, none);
head_armor_lookup_test(219) ->
   head_armor_lookup_test(219, <<"black knight helm">>, <<"黒騎士の兜"/utf8>>, 5, none);
head_armor_lookup_test(220) ->
   head_armor_lookup_test(220, <<"giant helm">>, <<"巨人の兜"/utf8>>, 6.3, none);
head_armor_lookup_test(221) ->
   head_armor_lookup_test(221, <<"six-eyed helm of the channelers"/utf8>>, <<"伝道者の六目兜"/utf8>>, 3.9, none);
head_armor_lookup_test(222) ->
   head_armor_lookup_test(222, <<"painting guardian hood">>, <<"絵画守りの頭巾"/utf8>>, 0.6, none);
head_armor_lookup_test(223) ->
   head_armor_lookup_test(223, <<"fang boar helm">>, <<"牙猪の兜"/utf8>>, 8, none);
head_armor_lookup_test(224) ->
   head_armor_lookup_test(224, <<"gargoyle helm">>, <<"ガーゴイルの兜"/utf8>>, 3.5, none);
head_armor_lookup_test(225) ->
   head_armor_lookup_test(225, <<"golem helm">>, <<"ゴーレムヘルム"/utf8>>, 6.3, none);
head_armor_lookup_test(226) ->
   head_armor_lookup_test(226, <<"smough`s helm">>, <<"スモウの兜"/utf8>>, 6.8, none);
head_armor_lookup_test(227) ->
   head_armor_lookup_test(227, <<"ornstein`s helm">>, <<"オーンスタインの兜"/utf8>>, 5.5, none);
head_armor_lookup_test(228) ->
   head_armor_lookup_test(228, <<"crown of the great lord">>, <<"大王の王冠"/utf8>>, 3, none);
head_armor_lookup_test(229) ->
   head_armor_lookup_test(229, <<"dark mask">>, <<"闇の仮面"/utf8>>, 3.8, none);
head_armor_lookup_test(230) ->
   head_armor_lookup_test(230, <<"helm of thorns">>, <<"トゲの兜"/utf8>>, 4.1, none);
head_armor_lookup_test(231) ->
   head_armor_lookup_test(231, <<"helm of favor">>, <<"寵愛の兜"/utf8>>, 4.5, none);
head_armor_lookup_test(232) ->
   head_armor_lookup_test(232, <<"crown of the dark sun">>, <<"陰の太陽の王冠"/utf8>>, 3, none);
head_armor_lookup_test(233) ->
   head_armor_lookup_test(233, <<"paladin helm">>, <<"聖騎士の兜"/utf8>>, 5, none);
head_armor_lookup_test(234) ->
   head_armor_lookup_test(234, <<"stone helm">>, <<"石の兜"/utf8>>, 6.8, none);
head_armor_lookup_test(235) ->
   head_armor_lookup_test(235, <<"havel`s helm">>, <<"ハベルの兜"/utf8>>, 7.5, none);
head_armor_lookup_test(236) ->
   head_armor_lookup_test(236, <<"xanthous crown">>, <<"黄衣の冠"/utf8>>, 5, none);
head_armor_lookup_test(237) ->
   head_armor_lookup_test(237, <<"mask of the father">>, <<"父の仮面"/utf8>>, 1.2, {value, 1.05});
head_armor_lookup_test(238) ->
   head_armor_lookup_test(238, <<"mask of the mother">>, <<"母の仮面"/utf8>>, 1.2, none);
head_armor_lookup_test(239) ->
   head_armor_lookup_test(239, <<"mask of the child">>, <<"子の仮面"/utf8>>, 1.2, none);
head_armor_lookup_test(240) ->
   head_armor_lookup_test(240, <<"sack">>, <<"ずた袋"/utf8>>, 0.6, none);
head_armor_lookup_test(241) ->
   head_armor_lookup_test(241, <<"royal helm">>, <<"王族の兜"/utf8>>, 4.5, none);
head_armor_lookup_test(242) ->
   head_armor_lookup_test(242, <<"sunlight maggot">>, <<"太陽虫"/utf8>>, 1.4, none);
head_armor_lookup_test(243) ->
   head_armor_lookup_test(243, <<"symbol of avarice">>, <<"貪欲者の烙印"/utf8>>, 10, none);
head_armor_lookup_test(244) ->
   head_armor_lookup_test(244, <<"guardian helm">>, <<"守護者の兜"/utf8>>, 7.2, none);
head_armor_lookup_test(245) ->
   head_armor_lookup_test(245, <<"porcelain mask">>, <<"白磁の仮面"/utf8>>, 2.5, none);
head_armor_lookup_test(246) ->
   head_armor_lookup_test(246, <<"bloated head">>, <<"肥大した頭部"/utf8>>, 2.5, none);
head_armor_lookup_test(247) ->
   head_armor_lookup_test(247, <<"bloated sorcerer head">>, <<"肥大した魔術師の頭部"/utf8>>, 2.2, none);
head_armor_lookup_test(248) ->
   head_armor_lookup_test(248, <<"helm of artorias">>, <<"アルトリウスの兜"/utf8>>, 4.2, none);
head_armor_lookup_test(249) ->
   head_armor_lookup_test(249, <<"snickering top hat">>, <<"微笑みロングハット"/utf8>>, 2, none);
head_armor_lookup_test(250) ->
   head_armor_lookup_test(250, <<"gough`s helm">>, <<"ゴーの兜"/utf8>>, 6.5, none).


-spec armor_lookup_test(
        dss_equipment:id(),
        unicode:unicode_binary(),
        unicode:unicode_binary(),
        float() | pos_integer(),
        dss_maybe:maybe(pos_integer())
    ) -> dss_maybe:maybe(pos_integer()).
armor_lookup_test(EquipmentID, ArmorType, Ename, Jname, Weight) ->
    ct:pal("EquipmentID: ~p~n", [EquipmentID]),
    {value, Equipment} = dss_equipment:lookup(ArmorType, EquipmentID),
    EquipmentID  = maps:get(id, Equipment),
    Ename        = maps:get(english, maps:get(name, Equipment)),
    Jname        = maps:get(japanese, maps:get(name, Equipment)),
    Weight       = maps:get(weight, Equipment).


-spec chest_armor_lookup_test(pos_integer()) -> float().
chest_armor_lookup_test(251) ->
    armor_lookup_test(251, chest_armor, <<"hard leather armor">>, <<"ハードレザーアーマー"/utf8>>, 5.9);
chest_armor_lookup_test(252) ->
    armor_lookup_test(252, chest_armor, <<"chain armor">>, <<"チェインアーマー"/utf8>>, 6);
chest_armor_lookup_test(253) ->
    armor_lookup_test(253, chest_armor, <<"knight armor">>, <<"騎士の鎧"/utf8>>, 10.9);
chest_armor_lookup_test(254) ->
    armor_lookup_test(254, chest_armor, <<"elite knight armor">>, <<"上級騎士の鎧"/utf8>>, 11.7);
chest_armor_lookup_test(255) ->
    armor_lookup_test(255, chest_armor, <<"wanderer coat">>, <<"放浪のコート"/utf8>>, 3.5);
chest_armor_lookup_test(256) ->
    armor_lookup_test(256, chest_armor, <<"black leather armor">>, <<"黒革の鎧"/utf8>>, 3.1);
chest_armor_lookup_test(257) ->
    armor_lookup_test(257, chest_armor, <<"brigand armor">>, <<"山賊の鎧"/utf8>>, 3.1);
chest_armor_lookup_test(258) ->
    armor_lookup_test(258, chest_armor, <<"leather armor">>, <<"レザーアーマー"/utf8>>, 4.7);
chest_armor_lookup_test(259) ->
    armor_lookup_test(259, chest_armor, <<"sorcerer cloak">>, <<"魔術師のコート"/utf8>>, 2.3);
chest_armor_lookup_test(260) ->
    armor_lookup_test(260, chest_armor, <<"black sorcerer cloak">>, <<"魔術師の黒コート"/utf8>>, 1.8);
chest_armor_lookup_test(261) ->
    armor_lookup_test(261, chest_armor, <<"tattered cloth robe">>, <<"ボロ布のローブ"/utf8>>, 2.7);
chest_armor_lookup_test(262) ->
    armor_lookup_test(262, chest_armor, <<"holy robe">>, <<"聖職の上衣"/utf8>>, 4);
chest_armor_lookup_test(263) ->
    armor_lookup_test(263, chest_armor, <<"cleric armor">>, <<"聖職の鎧"/utf8>>, 12.5);
chest_armor_lookup_test(264) ->
    armor_lookup_test(264, chest_armor, <<"armor of the sun">>, <<"太陽印の鎧"/utf8>>, 9);
chest_armor_lookup_test(265) ->
    armor_lookup_test(265, chest_armor, <<"black iron armor">>, <<"黒鉄の鎧"/utf8>>, 15.6);
chest_armor_lookup_test(266) ->
    armor_lookup_test(266, chest_armor, <<"armor of the glorious">>, <<"名誉者の鎧"/utf8>>, 13.3);
chest_armor_lookup_test(267) ->
    armor_lookup_test(267, chest_armor, <<"catarina armor">>, <<"カタリナアーマー"/utf8>>, 11.7);
chest_armor_lookup_test(268) ->
    armor_lookup_test(268, chest_armor, <<"crystalline armor">>, <<"結晶付きの鎧"/utf8>>, 10.9);
chest_armor_lookup_test(269) ->
    armor_lookup_test(269, chest_armor, <<"brass armor">>, <<"真鍮の鎧"/utf8>>, 10.9);
chest_armor_lookup_test(270) ->
    armor_lookup_test(270, chest_armor, <<"sage robe">>, <<"賢者のローブ"/utf8>>, 4);
chest_armor_lookup_test(271) ->
    armor_lookup_test(271, chest_armor, <<"crimson robe">>, <<"紅のローブ"/utf8>>, 3.9);
chest_armor_lookup_test(272) ->
    armor_lookup_test(272, chest_armor, <<"antiquated dress">>, <<"古めかしいドレス"/utf8>>, 1);
chest_armor_lookup_test(273) ->
    armor_lookup_test(273, chest_armor, <<"witch cloak">>, <<"魔女のコート"/utf8>>, 4);
chest_armor_lookup_test(274) ->
    armor_lookup_test(274, chest_armor, <<"gold-hemmed black cloak">>, <<"黒金糸のローブ"/utf8>>, 3.5);
chest_armor_lookup_test(275) ->
    armor_lookup_test(275, chest_armor, <<"black cleric robe">>, <<"黒の聖職衣"/utf8>>, 3.9);
chest_armor_lookup_test(276) ->
    armor_lookup_test(276, chest_armor, <<"maiden robe">>, <<"聖女の上衣"/utf8>>, 2);
chest_armor_lookup_test(277) ->
    armor_lookup_test(277, chest_armor, <<"dingy robe">>, <<"うす汚れた上衣"/utf8>>, 3);
chest_armor_lookup_test(278) ->
    armor_lookup_test(278, chest_armor, <<"eastern armor">>, <<"東国の鎧"/utf8>>, 12.3);
chest_armor_lookup_test(279) ->
    armor_lookup_test(279, chest_armor, <<"shadow garb">>, <<"影の上衣"/utf8>>, 2.3);
chest_armor_lookup_test(280) ->
    armor_lookup_test(280, chest_armor, <<"hollow warrior armor">>, <<"亡者戦士の鎧"/utf8>>, 6.6);
chest_armor_lookup_test(281) ->
    armor_lookup_test(281, chest_armor, <<"hollow soldier armor">>, <<"亡者兵士の鎧"/utf8>>, 7.8);
chest_armor_lookup_test(282) ->
    armor_lookup_test(282, chest_armor, <<"balder armor">>, <<"バルデルの鎧"/utf8>>, 10.9);
chest_armor_lookup_test(283) ->
    armor_lookup_test(283, chest_armor, <<"steel armor">>, <<"鋼鉄の鎧"/utf8>>, 14);
chest_armor_lookup_test(284) ->
    armor_lookup_test(284, chest_armor, <<"hollow thief`s leather armor">>, <<"亡者盗賊の革鎧"/utf8>>, 2.8);
chest_armor_lookup_test(285) ->
    armor_lookup_test(285, chest_armor, <<"silver knight armor">>, <<"銀騎士の鎧"/utf8>>, 12);
chest_armor_lookup_test(286) ->
    armor_lookup_test(286, chest_armor, <<"black knight armor">>, <<"黒騎士の鎧"/utf8>>, 13);
chest_armor_lookup_test(287) ->
    armor_lookup_test(287, chest_armor, <<"giant armor">>, <<"巨人の鎧"/utf8>>, 16.4);
chest_armor_lookup_test(288) ->
    armor_lookup_test(288, chest_armor, <<"robe of the channelers">>, <<"伝道者の聖衣"/utf8>>, 10.1);
chest_armor_lookup_test(289) ->
    armor_lookup_test(289, chest_armor, <<"painting guardian robe">>, <<"絵画守りの長衣"/utf8>>, 1.6);
chest_armor_lookup_test(290) ->
    armor_lookup_test(290, chest_armor, <<"golem armor">>, <<"ゴーレムアーマー"/utf8>>, 16.4);
chest_armor_lookup_test(291) ->
    armor_lookup_test(291, chest_armor, <<"smough`s armor">>, <<"スモウの鎧"/utf8>>, 17.6);
chest_armor_lookup_test(292) ->
    armor_lookup_test(292, chest_armor, <<"ornstein`s armor">>, <<"オーンスタインの鎧"/utf8>>, 12);
chest_armor_lookup_test(293) ->
    armor_lookup_test(293, chest_armor, <<"moonlight robe">>, <<"月光の長衣"/utf8>>, 3.1);
chest_armor_lookup_test(294) ->
    armor_lookup_test(294, chest_armor, <<"robe of the great lord">>, <<"大王の長衣"/utf8>>, 6);
chest_armor_lookup_test(295) ->
    armor_lookup_test(295, chest_armor, <<"dark armor">>, <<"闇の鎧"/utf8>>, 9.8);
chest_armor_lookup_test(296) ->
    armor_lookup_test(296, chest_armor, <<"armor of thorns">>, <<"トゲの鎧"/utf8>>, 10.5);
chest_armor_lookup_test(297) ->
    armor_lookup_test(297, chest_armor, <<"embraced armor of favor">>, <<"寵愛の抱かれ鎧"/utf8>>, 11.7);
chest_armor_lookup_test(298) ->
    armor_lookup_test(298, chest_armor, <<"paladin armor">>, <<"聖騎士の鎧"/utf8>>, 12.9);
chest_armor_lookup_test(299) ->
    armor_lookup_test(299, chest_armor, <<"stone armor">>, <<"石の鎧"/utf8>>, 17.6);
chest_armor_lookup_test(300) ->
    armor_lookup_test(300, chest_armor, <<"havel`s armor">>, <<"ハベルの鎧"/utf8>>, 19.5);
chest_armor_lookup_test(301) ->
    armor_lookup_test(301, chest_armor, <<"xanthous overcoat">>, <<"黄の上衣"/utf8>>, 3.9);
chest_armor_lookup_test(302) ->
    armor_lookup_test(302, chest_armor, <<"guardian armor">>, <<"守護者の鎧"/utf8>>, 17);
chest_armor_lookup_test(303) ->
    armor_lookup_test(303, chest_armor, <<"lord`s blade robe">>, <<"王刃の長衣"/utf8>>, 6.4);
chest_armor_lookup_test(304) ->
    armor_lookup_test(304, chest_armor, <<"armor of artorias">>, <<"アルトリウスの鎧"/utf8>>, 10.5);
chest_armor_lookup_test(305) ->
    armor_lookup_test(305, chest_armor, <<"chester`s long coat">>, <<"チェスターのロングコート"/utf8>>, 4.5);
chest_armor_lookup_test(306) ->
    armor_lookup_test(306, chest_armor, <<"gough`s armor">>, <<"ゴーの鎧"/utf8>>, 13).


-spec hand_armor_lookup_test(pos_integer()) -> float().
hand_armor_lookup_test(307) ->
    armor_lookup_test(307, hand_armor, <<"hard leather gauntlets">>, <<"ハードレザーガントレット"/utf8>>, 3.5);
hand_armor_lookup_test(308) ->
    armor_lookup_test(308, hand_armor, <<"leather gauntlets">>, <<"レザーガントレット"/utf8>>, 3.6);
hand_armor_lookup_test(309) ->
    armor_lookup_test(309, hand_armor, <<"knight gauntlets">>, <<"騎士の手甲"/utf8>>, 3.5);
hand_armor_lookup_test(310) ->
    armor_lookup_test(310, hand_armor, <<"elite knight gauntlets">>, <<"上級騎士の手甲"/utf8>>, 3.7);
hand_armor_lookup_test(311) ->
    armor_lookup_test(311, hand_armor, <<"wanderer manchette">>, <<"放浪のマンシェット"/utf8>>, 2.1);
hand_armor_lookup_test(312) ->
    armor_lookup_test(312, hand_armor, <<"black leather gloves">>, <<"黒革の手袋"/utf8>>, 1.8);
hand_armor_lookup_test(313) ->
    armor_lookup_test(313, hand_armor, <<"brigand gauntlets">>, <<"山賊の篭手"/utf8>>, 1.8);
hand_armor_lookup_test(314) ->
    armor_lookup_test(314, hand_armor, <<"leather gloves">>, <<"レザーグローブ"/utf8>>, 2.8);
hand_armor_lookup_test(315) ->
    armor_lookup_test(315, hand_armor, <<"sorcerer gauntlets">>, <<"魔術師のガントレット"/utf8>>, 1.4);
hand_armor_lookup_test(316) ->
    armor_lookup_test(316, hand_armor, <<"black sorcerer gauntlets">>, <<"魔術師の黒ガントレット"/utf8>>, 1);
hand_armor_lookup_test(317) ->
    armor_lookup_test(317, hand_armor, <<"tattered cloth manchette">>, <<"ボロ布のマンシェット"/utf8>>, 1.6);
hand_armor_lookup_test(318) ->
    armor_lookup_test(318, hand_armor, <<"traveling gloves (creric)">>, <<"旅の手袋"/utf8>>, 0.7);
hand_armor_lookup_test(319) ->
    armor_lookup_test(319, hand_armor, <<"cleric gauntlets">>, <<"聖職の手甲"/utf8>>, 7.4);
hand_armor_lookup_test(320) ->
    armor_lookup_test(320, hand_armor, <<"iron bracelet">>, <<"鉄の腕輪"/utf8>>, 4.3);
hand_armor_lookup_test(321) ->
    armor_lookup_test(321, hand_armor, <<"black iron gauntlets">>, <<"黒鉄の手甲"/utf8>>, 9.2);
hand_armor_lookup_test(322) ->
    armor_lookup_test(322, hand_armor, <<"gauntlets of the vanquisher">>, <<"獲得者の篭手"/utf8>>, 3);
hand_armor_lookup_test(323) ->
    armor_lookup_test(323, hand_armor, <<"catarina gauntlets">>, <<"カタリナガントレット"/utf8>>, 6.9);
hand_armor_lookup_test(324) ->
    armor_lookup_test(324, hand_armor, <<"crystalline gauntlets">>, <<"結晶付きの手甲"/utf8>>, 6.4);
hand_armor_lookup_test(325) ->
    armor_lookup_test(325, hand_armor, <<"brass gauntlets">>, <<"真鍮の手甲"/utf8>>, 5.4);
hand_armor_lookup_test(326) ->
    armor_lookup_test(326, hand_armor, <<"traveling gloves (big hat)">>, <<"旅の長手袋"/utf8>>, 1.6);
hand_armor_lookup_test(327) ->
    armor_lookup_test(327, hand_armor, <<"crimson gloves">>, <<"紅のグローブ"/utf8>>, 0.8);
hand_armor_lookup_test(328) ->
    armor_lookup_test(328, hand_armor, <<"antiquated gloves">>, <<"古めかしいロンググローブ"/utf8>>, 0.6);
hand_armor_lookup_test(329) ->
    armor_lookup_test(329, hand_armor, <<"witch gloves">>, <<"魔女のグローブ"/utf8>>, 1.7);
hand_armor_lookup_test(330) ->
    armor_lookup_test(330, hand_armor, <<"gold-hemmed black gloves">>, <<"黒金糸のロンググローブ"/utf8>>, 1.2);
hand_armor_lookup_test(331) ->
    armor_lookup_test(331, hand_armor, <<"black manchette">>, <<"黒のマンシェット"/utf8>>, 1.8);
hand_armor_lookup_test(332) ->
    armor_lookup_test(332, hand_armor, <<"maiden gloves">>, <<"聖女の手袋"/utf8>>, 1.2);
hand_armor_lookup_test(333) ->
    armor_lookup_test(333, hand_armor, <<"dingy gloves">>, <<"うす汚れた手袋"/utf8>>, 1.2);
hand_armor_lookup_test(334) ->
    armor_lookup_test(334, hand_armor, <<"eastern gauntlets">>, <<"東国の手甲"/utf8>>, 1.5);
hand_armor_lookup_test(335) ->
    armor_lookup_test(335, hand_armor, <<"shadow gauntlets">>, <<"影の手甲"/utf8>>, 1.4);
hand_armor_lookup_test(336) ->
    armor_lookup_test(336, hand_armor, <<"balder gauntlets">>, <<"バルデルの手甲"/utf8>>, 3.5);
hand_armor_lookup_test(337) ->
    armor_lookup_test(337, hand_armor, <<"steel gauntlets">>, <<"鋼鉄の手甲"/utf8>>, 8.3);
hand_armor_lookup_test(338) ->
    armor_lookup_test(338, hand_armor, <<"silver knight gauntlets">>, <<"銀騎士の手甲"/utf8>>, 5.5);
hand_armor_lookup_test(339) ->
    armor_lookup_test(339, hand_armor, <<"black knight gauntlets">>, <<"黒騎士の手甲"/utf8>>, 6);
hand_armor_lookup_test(340) ->
    armor_lookup_test(340, hand_armor, <<"giant gauntlets">>, <<"巨人の手甲"/utf8>>, 9.7);
hand_armor_lookup_test(341) ->
    armor_lookup_test(341, hand_armor, <<"gauntlets of the channelers">>, <<"伝道者の篭手"/utf8>>, 3);
hand_armor_lookup_test(342) ->
    armor_lookup_test(342, hand_armor, <<"painting guardian gloves">>, <<"絵画守りの長手袋"/utf8>>, 0.9);
hand_armor_lookup_test(343) ->
    armor_lookup_test(343, hand_armor, <<"golem gauntlets">>, <<"ゴーレムガントレット"/utf8>>, 9.7);
hand_armor_lookup_test(344) ->
    armor_lookup_test(344, hand_armor, <<"smough`s gauntlets">>, <<"スモウの手甲"/utf8>>, 10.4);
hand_armor_lookup_test(345) ->
    armor_lookup_test(345, hand_armor, <<"ornstein`s gauntlets">>, <<"オーンスタインの手甲"/utf8>>, 4);
hand_armor_lookup_test(346) ->
    armor_lookup_test(346, hand_armor, <<"gauntlets of favor">>, <<"寵愛の手甲"/utf8>>, 5);
hand_armor_lookup_test(347) ->
    armor_lookup_test(347, hand_armor, <<"paladin gauntlets">>, <<"聖騎士の手甲"/utf8>>, 7.6);
hand_armor_lookup_test(348) ->
    armor_lookup_test(348, hand_armor, <<"stone gauntlets">>, <<"石の手甲"/utf8>>, 10.4);
hand_armor_lookup_test(349) ->
    armor_lookup_test(349, hand_armor, <<"havel`s gauntlets">>, <<"ハベルの手甲"/utf8>>, 11.5);
hand_armor_lookup_test(350) ->
    armor_lookup_test(350, hand_armor, <<"xanthous gloves">>, <<"黄衣の長手袋"/utf8>>, 2.3);
hand_armor_lookup_test(351) ->
    armor_lookup_test(351, hand_armor, <<"dark gauntlets">>, <<"闇の手甲"/utf8>>, 5.8);
hand_armor_lookup_test(352) ->
    armor_lookup_test(352, hand_armor, <<"gauntlets of thorns">>, <<"トゲの手甲"/utf8>>, 6.2);
hand_armor_lookup_test(353) ->
    armor_lookup_test(353, hand_armor, <<"moonlight gloves">>, <<"月光の長手袋"/utf8>>, 0.5);
hand_armor_lookup_test(354) ->
    armor_lookup_test(354, hand_armor, <<"bracelet of the great lord">>, <<"大王の腕輪"/utf8>>, 2.8);
hand_armor_lookup_test(355) ->
    armor_lookup_test(355, hand_armor, <<"guardian gauntlets">>, <<"守護者の手甲"/utf8>>, 10.4);
hand_armor_lookup_test(356) ->
    armor_lookup_test(356, hand_armor, <<"lord`s blade gloves">>, <<"王刃の長手袋"/utf8>>, 2.8);
hand_armor_lookup_test(357) ->
    armor_lookup_test(357, hand_armor, <<"gauntlets of artorias">>, <<"アルトリウスの手甲"/utf8>>, 4.6);
hand_armor_lookup_test(358) ->
    armor_lookup_test(358, hand_armor, <<"chester`s gloves">>, <<"チェスターのグローブ"/utf8>>, 1.5);
hand_armor_lookup_test(359) ->
    armor_lookup_test(359, hand_armor, <<"gough`s gauntlets">>, <<"ゴーの手甲"/utf8>>, 7).


-spec leg_armor_lookup_test(pos_integer()) -> float().
leg_armor_lookup_test(360) ->
    armor_lookup_test(360, leg_armor, <<"hard leather boots">>, <<"ハードレザーブーツ"/utf8>>, 3.5);
leg_armor_lookup_test(361) ->
    armor_lookup_test(361, leg_armor, <<"chain leggings">>, <<"チェインレギンス"/utf8>>, 4.6);
leg_armor_lookup_test(362) ->
    armor_lookup_test(362, leg_armor, <<"knight leggings">>, <<"騎士の足甲"/utf8>>, 6.4);
leg_armor_lookup_test(363) ->
    armor_lookup_test(363, leg_armor, <<"elite knight leggings">>, <<"上級騎士の足甲"/utf8>>, 6.9);
leg_armor_lookup_test(364) ->
    armor_lookup_test(364, leg_armor, <<"wanderer boots">>, <<"放浪のブーツ"/utf8>>, 2.1);
leg_armor_lookup_test(365) ->
    armor_lookup_test(365, leg_armor, <<"black leather boots">>, <<"黒革のブーツ"/utf8>>, 3);
leg_armor_lookup_test(366) ->
    armor_lookup_test(366, leg_armor, <<"brigand trousers">>, <<"山賊のズボン"/utf8>>, 1.8);
leg_armor_lookup_test(367) ->
    armor_lookup_test(367, leg_armor, <<"leather boots">>, <<"レザーブーツ"/utf8>>, 2.8);
leg_armor_lookup_test(368) ->
    armor_lookup_test(368, leg_armor, <<"sorcerer boots">>, <<"魔術師のブーツ"/utf8>>, 1.4);
leg_armor_lookup_test(369) ->
    armor_lookup_test(369, leg_armor, <<"black sorcerer boots">>, <<"魔術師の黒ブーツ"/utf8>>, 1);
leg_armor_lookup_test(370) ->
    armor_lookup_test(370, leg_armor, <<"heavy boots">>, <<"厚手のブーツ"/utf8>>, 1.6);
leg_armor_lookup_test(371) ->
    armor_lookup_test(371, leg_armor, <<"holy trousers">>, <<"聖職のズボン"/utf8>>, 2);
leg_armor_lookup_test(372) ->
    armor_lookup_test(372, leg_armor, <<"cleric leggings">>, <<"聖職の足甲"/utf8>>, 7.4);
leg_armor_lookup_test(373) ->
    armor_lookup_test(373, leg_armor, <<"iron leggings">>, <<"鉄の足甲"/utf8>>, 5.3);
leg_armor_lookup_test(374) ->
    armor_lookup_test(374, leg_armor, <<"black iron leggings">>, <<"黒鉄の足甲"/utf8>>, 9.2);
leg_armor_lookup_test(375) ->
    armor_lookup_test(375, leg_armor, <<"boots of the explorer">>, <<"探索者の長靴"/utf8>>, 4);
leg_armor_lookup_test(376) ->
    armor_lookup_test(376, leg_armor, <<"catarina leggings">>, <<"カタリナレギンス"/utf8>>, 6.9);
leg_armor_lookup_test(377) ->
    armor_lookup_test(377, leg_armor, <<"crystalline leggings">>, <<"結晶付きの足甲"/utf8>>, 6.4);
leg_armor_lookup_test(378) ->
    armor_lookup_test(378, leg_armor, <<"brass leggings">>, <<"真鍮の足甲"/utf8>>, 6.4);
leg_armor_lookup_test(379) ->
    armor_lookup_test(379, leg_armor, <<"traveling boots">>, <<"旅の靴"/utf8>>, 1.6);
leg_armor_lookup_test(380) ->
    armor_lookup_test(380, leg_armor, <<"crimson waistcloth">>, <<"紅の腰巻き"/utf8>>, 3.5);
leg_armor_lookup_test(381) ->
    armor_lookup_test(381, leg_armor, <<"antiquated skirt">>, <<"古めかしいスカート"/utf8>>, 3);
leg_armor_lookup_test(382) ->
    armor_lookup_test(382, leg_armor, <<"witch skirt">>, <<"魔女のスカート"/utf8>>, 2.5);
leg_armor_lookup_test(383) ->
    armor_lookup_test(383, leg_armor, <<"gold-hemmed black skirt">>, <<"黒金糸のスカート"/utf8>>, 3);
leg_armor_lookup_test(384) ->
    armor_lookup_test(384, leg_armor, <<"black tights">>, <<"黒のタイツ"/utf8>>, 2.8);
leg_armor_lookup_test(385) ->
    armor_lookup_test(385, leg_armor, <<"maiden skirt">>, <<"聖女のスカート"/utf8>>, 2.5);
leg_armor_lookup_test(386) ->
    armor_lookup_test(386, leg_armor, <<"blood-stained skirt">>, <<"血濡れたスカート"/utf8>>, 2.5);
leg_armor_lookup_test(387) ->
    armor_lookup_test(387, leg_armor, <<"eastern leggings">>, <<"東国の足甲"/utf8>>, 4);
leg_armor_lookup_test(388) ->
    armor_lookup_test(388, leg_armor, <<"shadow leggings">>, <<"影の足甲"/utf8>>, 1.4);
leg_armor_lookup_test(389) ->
    armor_lookup_test(389, leg_armor, <<"hollow warrior waistcloth">>, <<"亡者戦士の腰巻き"/utf8>>, 1.4);
leg_armor_lookup_test(390) ->
    armor_lookup_test(390, leg_armor, <<"hollow soldier waistcloth">>, <<"亡者兵士の腰巻き"/utf8>>, 1.5);
leg_armor_lookup_test(391) ->
    armor_lookup_test(391, leg_armor, <<"balder leggings">>, <<"バルデルの足甲"/utf8>>, 6.4);
leg_armor_lookup_test(392) ->
    armor_lookup_test(392, leg_armor, <<"steel leggings">>, <<"鋼鉄の足甲"/utf8>>, 8.3);
leg_armor_lookup_test(393) ->
    armor_lookup_test(393, leg_armor, <<"hollow thief`s tights">>, <<"亡者盗賊のタイツ"/utf8>>, 1.7);
leg_armor_lookup_test(394) ->
    armor_lookup_test(394, leg_armor, <<"silver knight leggings">>, <<"銀騎士の足甲"/utf8>>, 6.5);
leg_armor_lookup_test(395) ->
    armor_lookup_test(395, leg_armor, <<"black knight leggings">>, <<"黒騎士の足甲"/utf8>>, 7);
leg_armor_lookup_test(396) ->
    armor_lookup_test(396, leg_armor, <<"giant leggings">>, <<"巨人の足甲"/utf8>>, 9.7);
leg_armor_lookup_test(397) ->
    armor_lookup_test(397, leg_armor, <<"waistcloth of the channelers">>, <<"伝道者の腰巻き"/utf8>>, 6);
leg_armor_lookup_test(398) ->
    armor_lookup_test(398, leg_armor, <<"painting guardian waistcloth">>, <<"絵画守りの腰巻き"/utf8>>, 4);
leg_armor_lookup_test(399) ->
    armor_lookup_test(399, leg_armor, <<"golem leggings">>, <<"ゴーレムレギンス"/utf8>>, 9.7);
leg_armor_lookup_test(400) ->
    armor_lookup_test(400, leg_armor, <<"smough`s leggings">>, <<"スモウの足甲"/utf8>>, 10.4);
leg_armor_lookup_test(401) ->
    armor_lookup_test(401, leg_armor, <<"ornstein`s leggings">>, <<"オーンスタインの足甲"/utf8>>, 7);
leg_armor_lookup_test(402) ->
    armor_lookup_test(402, leg_armor, <<"leggings of favor">>, <<"寵愛の足甲"/utf8>>, 6.9);
leg_armor_lookup_test(403) ->
    armor_lookup_test(403, leg_armor, <<"paladin leggings">>, <<"聖騎士の足甲"/utf8>>, 7.6);
leg_armor_lookup_test(404) ->
    armor_lookup_test(404, leg_armor, <<"stone leggings">>, <<"石の足甲"/utf8>>, 10.4);
leg_armor_lookup_test(405) ->
    armor_lookup_test(405, leg_armor, <<"havel`s leggings">>, <<"ハベルの足甲"/utf8>>, 11.5);
leg_armor_lookup_test(406) ->
    armor_lookup_test(406, leg_armor, <<"xanthous waistcloth">>, <<"黄衣の腰巻き"/utf8>>, 4);
leg_armor_lookup_test(407) ->
    armor_lookup_test(407, leg_armor, <<"dark leggings">>, <<"闇の足甲"/utf8>>, 5.8);
leg_armor_lookup_test(408) ->
    armor_lookup_test(408, leg_armor, <<"leggings of thorns">>, <<"トゲの足甲"/utf8>>, 6.2);
leg_armor_lookup_test(409) ->
    armor_lookup_test(409, leg_armor, <<"moonlight waistcloth">>, <<"月光の腰巻き"/utf8>>, 1.8);
leg_armor_lookup_test(410) ->
    armor_lookup_test(410, leg_armor, <<"anklet of the great lord">>, <<"大王の脚輪"/utf8>>, 2.8);
leg_armor_lookup_test(411) ->
    armor_lookup_test(411, leg_armor, <<"guardian leggings">>, <<"守護者の足甲"/utf8>>, 10.4);
leg_armor_lookup_test(412) ->
    armor_lookup_test(412, leg_armor, <<"lord`s blade waistcloth">>, <<"王刃の腰巻き"/utf8>>, 5);
leg_armor_lookup_test(413) ->
    armor_lookup_test(413, leg_armor, <<"leggings of artorias">>, <<"アルトリウスの足甲"/utf8>>, 5.5);
leg_armor_lookup_test(414) ->
    armor_lookup_test(414, leg_armor, <<"chester`s trousers">>, <<"チェスターのズボン"/utf8>>, 3.2);
leg_armor_lookup_test(415) ->
    armor_lookup_test(415, leg_armor, <<"gough`s leggings">>, <<"ゴーの足甲"/utf8>>, 8).


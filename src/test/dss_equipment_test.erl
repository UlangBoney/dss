-module(dss_equipment_test).

-include_lib("eunit/include/eunit.hrl").


-spec equipment_test() -> ok.
equipment_test() ->
    ok = lookup_test(),
    ok = list_test().


-spec lookup_test() -> ok.
lookup_test() ->
     ok = dagger_lookup_test(),
     ok = straight_sword_lookup_test(),
     ok = greats_sword_lookup_test(),
     ok = ultra_greatsword_lookup_test().


-spec list_test() -> ok.
list_test() ->
     ok = dagger_list_test(),
     ok = straight_sword_list_test(),
     ok = greats_sword_list_test().


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


-spec weapon_lookup_test(
        dss_equipment:id(),
        dss_equipment:equipment_type(),
        unicode:unicode_binary(),
        unicode:unicode_binary(),
        float(),
        dss_equipment:requirement()
    ) -> true.
weapon_lookup_test(EquipmentID, EqpType, Ename, Jname, Weigth, #{strenght := Strength, dexterity := Dexterity, intelligence := Intelligence, faith := Faith}) ->
    {value, Equipment} = dss_equipment:lookup(EqpType, EquipmentID),
    EquipmentID  = maps:get(id, Equipment),
    EName        = maps:get(english, maps:get(name, Equipment)),
    JName        = maps:get(japanese, maps:get(name, Equipment)),
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


-spec dagger_lookup_test(pos_integer()) -> pos_integer().
dagger_lookup_test(1) ->
    Requirements = #{ strenght     => 5
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(1, dagger, <<"dagger">>, <<"ダガー">>, 0.5, Requirements);
dagger_lookup_test(2) ->
    Requirements = #{ strenght     => 5
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(2, dagger, <<"parrying dagger">>, <<"パリングダガー">>, 0.5, Requirements);
dagger_lookup_test(3) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(3, dagger, <<"bandit's knife">>, <<"盗賊の短刀">>, 1, Requirements);
dagger_lookup_test(4) ->
    Requirements = #{ strenght     => 5
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(4, dagger, <<"ghost blade">>, <<"亡霊の刃">>, 0.5, Requirements);
dagger_lookup_test(5) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 25
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(5, dagger, <<"dark silver tracer">>, <<"暗銀の残滅">>, 1, Requirements);
dagger_lookup_test(6) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 20
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(6, dagger, <<"priscilla's dagger">>, <<"プリシラの短剣">>, 1, Requirements).


-spec straight_sword_lookup_test(pos_integer()) -> pos_integer().
straight_sword_lookup_test(7) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(7, straight_sword, <<"short sword">>, <<"ショートソード">>, 2, Requirements);
straight_sword_lookup_test(8) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(8, straight_sword, <<"long sword">>, <<"ロングソード">>, 2, Requirements);
straight_sword_lookup_test(9) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(9, straight_sword, <<"broad sword">>, <<"ブロードソード">>, 3, Requirements);
straight_sword_lookup_test(10) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(10, straight_sword, <<"balder side sword">>, <<"バルデルの刺突直剣">>, 3, Requirements);
straight_sword_lookup_test(11) ->
    Requirements = #{ strenght     => 12
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(11, straight_sword, <<"sunlight straight sword">>, <<"太陽の直剣">>, 4, Requirements);
straight_sword_lookup_test(12) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 16
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(12, straight_sword, <<"dark sword">>, <<"ダークソード">>, 6, Requirements);
straight_sword_lookup_test(13) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(13, straight_sword, <<"barbed straight sword">>, <<"トゲの直剣">>, 3, Requirements);
straight_sword_lookup_test(14) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(14, straight_sword, <<"crystal straight sword">>, <<"結晶直剣">>, 6, Requirements);
straight_sword_lookup_test(15) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 22
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(15, straight_sword, <<"silver knight straight sword">>, <<"銀騎士の剣">>, 6, Requirements);
straight_sword_lookup_test(16) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 14},
    weapon_lookup_test(16, straight_sword, <<"astora's straight sword">>, <<"アストラの直剣">>, 3, Requirements);
straight_sword_lookup_test(17) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(17, straight_sword, <<"drake sword">>, <<"飛竜の剣">>, 6, Requirements);
straight_sword_lookup_test(18) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(18, straight_sword, <<"broken straight sword">>, <<"折れた直剣">>, 2, Requirements);
straight_sword_lookup_test(19) ->
    Requirements = #{ strenght     => 6
                    , dexterity    => 6
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(19, straight_sword, <<"straight sword hilt">>, <<"直剣の柄">>, 1, Requirements).


-spec greats_sword_lookup_test(pos_integer()) -> pos_integer().
greats_sword_lookup_test(20) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(20, greats_sword, <<"bastard sword">>, <<"バスタードソード">>, 6, Requirements);
greats_sword_lookup_test(21) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(21, greats_sword, <<"claymore">>, <<"クレイモア">>, 6, Requirements);
greats_sword_lookup_test(22) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(22, greats_sword, <<"man serpent greatsword">>, <<"蛇人の大剣">>, 10, Requirements);
greats_sword_lookup_test(23) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(23, greats_sword, <<"flamberge">>, <<"フランベルジェ">>, 6, Requirements);
greats_sword_lookup_test(24) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(24, greats_sword, <<"crystal greatsword">>, <<"結晶大剣">>, 8, Requirements);
greats_sword_lookup_test(25) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 18
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(25, greats_sword, <<"black knight sword">>, <<"黒騎士の剣">>, 8, Requirements);
greats_sword_lookup_test(26) ->
    Requirements = #{ strenght     => 40
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(26, greats_sword, <<"stone greatsword">>, <<"石の大剣">>, 18, Requirements);
greats_sword_lookup_test(27) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 18
                    , intelligence => 18
                    , faith        => 18},
    weapon_lookup_test(27, greats_sword, <<"greatsword of artorias">>, <<"アウトリウスの大剣">>, 10, Requirements);
greats_sword_lookup_test(28) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 18
                    , intelligence => 20
                    , faith        => 20},
    weapon_lookup_test(28, greats_sword, <<"greatsword of artorias (cursed)">>, <<"アウトリウスの大剣 (聖)">>, 10, Requirements);
greats_sword_lookup_test(29) ->
    Requirements = #{ strenght     => 22
                    , dexterity    => 18
                    , intelligence => 18
                    , faith        => 18},
    weapon_lookup_test(29, greats_sword, <<"abyss greatsword">>, <<"深淵の大剣">>, 9, Requirements);
greats_sword_lookup_test(30) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(30, greats_sword, <<"great lord greatsword">>, <<"大王の大剣">>, 8, Requirements);
greats_sword_lookup_test(31) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 10
                    , intelligence => 28
                    , faith        => 0},
    weapon_lookup_test(31, greats_sword, <<"moonlight greatsword">>, <<"月光の大剣">>, 6, Requirements);
greats_sword_lookup_test(32) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 16
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(32, greats_sword, <<"obsidian greatsword">>, <<"穀粒の大剣">>, 8, Requirements).


-spec ultra_greatsword_lookup_test(pos_integer()) -> pos_integer().
ultra_greatsword_lookup_test(33) ->
    Requirements = #{ strenght     => 28
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(33, ultra_greatsword, <<"greatsword">>, <<"グレートソード">>, 12, Requirements);
ultra_greatsword_lookup_test(34) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(34, ultra_greatsword, <<"zweihander">>, <<"ツヴァイヘンダー">>, 10, Requirements);
ultra_greatsword_lookup_test(35) ->
    Requirements = #{ strenght     => 40
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(35, ultra_greatsword, <<"demon great machete">>, <<"デーモンの大鉈">>, 18, Requirements);
ultra_greatsword_lookup_test(36) ->
    Requirements = #{ strenght     => 32
                    , dexterity    => 18
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(36, ultra_greatsword, <<"black knight greatsword">>, <<"黒騎士の大剣">>, 14, Requirements);
ultra_greatsword_lookup_test(37) ->
    Requirements = #{ strenght     => 50
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(37, ultra_greatsword, <<"dragon greatsword">>, <<"古竜の大剣">>, 24, Requirements).


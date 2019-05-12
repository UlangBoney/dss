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
     ok = ultra_greatsword_lookup_test(),
     ok = curved_sword_lookup_test(),
     ok = curved_greatsword_lookup_test(),
     ok = thrusting_sword_lookup_test(),
     ok = katana_lookup_test(),
     ok = axe_lookup_test(),
     ok = greataxe_lookup_test().


-spec list_test() -> ok.
list_test() ->
     ok = dagger_list_test(),
     ok = straight_sword_list_test(),
     ok = greats_sword_list_test(),
     ok = ultra_greatsword_list_test(),
     ok = curved_sword_list_test(),
     ok = curved_greatsword_list_test(),
     ok = thrusting_sword_list_test(),
     ok = katana_list_test(),
     ok = axe_list_test(),
     ok = greataxe_list_test().


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


-spec curved_sword_lookup_test(pos_integer()) -> pos_integer().
curved_sword_lookup_test(38) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(38, curved_sword, <<"scimitar">>, <<"シミター">>, 1.5, Requirements);
curved_sword_lookup_test(39) ->
    Requirements = #{ strenght     => 9
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(39, curved_sword, <<"falchion">>, <<"ファルシオン">>, 2.5, Requirements);
curved_sword_lookup_test(40) ->
    Requirements = #{ strenght     => 9
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(40, curved_sword, <<"shotel">>, <<"ショーテル">>, 2.5, Requirements);
curved_sword_lookup_test(41) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 20
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(41, curved_sword, <<"painting guardian sword">>, <<"絵画守りの曲刀">>, 1.5, Requirements);
curved_sword_lookup_test(42) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(42, curved_sword, <<"jagged ghost blade">>, <<"亡霊のギザギザ刃">>, 1.5, Requirements);
curved_sword_lookup_test(43) ->
    Requirements = #{ strenght     => 9
                    , dexterity    => 25
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(43, curved_sword, <<"gold tracer">>, <<"黄金の残光">>, 2, Requirements);
curved_sword_lookup_test(44) ->
    Requirements = #{ strenght     => 11
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(44, curved_sword, <<"quelaag's fury sword">>, <<"クラーグの魔剣">>, 3.5, Requirements).


-spec curved_greatsword_lookup_test(pos_integer()) -> pos_integer().
curved_greatsword_lookup_test(45) ->
    Requirements = #{ strenght     => 28
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(45, curved_greatsword, <<"murakumo">>, <<"ムラクモ">>, 12, Requirements);
curved_greatsword_lookup_test(46) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(46, curved_greatsword, <<"server">>, <<"生贄刀">>, 10, Requirements);
curved_greatsword_lookup_test(47) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 13
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(47, curved_greatsword, <<"gravelord sword">>, <<"墓王の剣">>, 10, Requirements).


-spec thrusting_sword_lookup_test(pos_integer()) -> pos_integer().
thrusting_sword_lookup_test(48) ->
    Requirements = #{ strenght     => 5
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(48, thrusting_sword, <<"mail breaker">>, <<"鎧貫き">>, 0.5, Requirements);
thrusting_sword_lookup_test(49) ->
    Requirements = #{ strenght     => 7
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(49, thrusting_sword, <<"rapier">>, <<"レイピア">>, 1.5, Requirements);
thrusting_sword_lookup_test(50) ->
    Requirements = #{ strenght     => 10
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(50, thrusting_sword, <<"estoc">>, <<"エストック">>, 3, Requirements);
thrusting_sword_lookup_test(51) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 20
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(51, thrusting_sword, <<"ricard's rapier">>, <<"リカールの刺剣">>, 2, Requirements);
thrusting_sword_lookup_test(52) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 16
                    , intelligence => 16
                    , faith        => 0},
    weapon_lookup_test(52, thrusting_sword, <<"velka's rapier">>, <<"ベルカの刺剣">>, 2, Requirements).


-spec katana_lookup_test(pos_integer()) -> pos_integer().
katana_lookup_test(53) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(53, katana, <<"uchigatana">>, <<"打刀">>, 5, Requirements);
katana_lookup_test(54) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 20
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(54, katana, <<"iaito">>, <<"居合い刀">>, 5, Requirements);
katana_lookup_test(55) ->
    Requirements = #{ strenght     => 20
                    , dexterity    => 16
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(55, katana, <<"washing pole">>, <<"物干し竿">>, 8, Requirements);
katana_lookup_test(56) ->
    Requirements = #{ strenght     => 16
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(56, katana, <<"chaos blade">>, <<"混沌の刃">>, 6, Requirements).


-spec axe_lookup_test(pos_integer()) -> pos_integer().
axe_lookup_test(57) ->
    Requirements = #{ strenght     => 8
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(57, axe, <<"hand axe">>, <<"ハンドアクス">>, 2, Requirements);
axe_lookup_test(58) ->
    Requirements = #{ strenght     => 12
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(58, axe, <<"battle axe">>, <<"バトルアクス">>, 4, Requirements);
axe_lookup_test(59) ->
    Requirements = #{ strenght     => 24
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(59, axe, <<"butcher knife">>, <<"肉断ち包丁">>, 10, Requirements);
axe_lookup_test(60) ->
    Requirements = #{ strenght     => 14
                    , dexterity    => 14
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(60, axe, <<"gargoyle tail axe">>, <<"ガーゴイルの尾斧">>, 5, Requirements);
axe_lookup_test(61) ->
    Requirements = #{ strenght     => 18
                    , dexterity    => 12
                    , intelligence => 0
                    , faith        => 16},
    weapon_lookup_test(61, axe, <<"crescent axe">>, <<"三日月斧">>, 7, Requirements);
axe_lookup_test(62) ->
    Requirements = #{ strenght     => 36
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(62, axe, <<"golem axe">>, <<"ゴーレムアクス">>, 16, Requirements).


-spec greataxe_lookup_test(pos_integer()) -> pos_integer().
greataxe_lookup_test(63) ->
    Requirements = #{ strenght     => 32
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(63, greataxe, <<"greataxe">>, <<"グレートアクス">>, 14, Requirements);
greataxe_lookup_test(64) ->
    Requirements = #{ strenght     => 46
                    , dexterity    => 0
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(64, greataxe, <<"demon's greataxe">>, <<"デーモンの大斧">>, 22, Requirements);
greataxe_lookup_test(65) ->
    Requirements = #{ strenght     => 48
                    , dexterity    => 10
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(65, greataxe, <<"stone greataxe">>, <<"岩の大斧">>, 24, Requirements);
greataxe_lookup_test(66) ->
    Requirements = #{ strenght     => 36
                    , dexterity    => 18
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(66, greataxe, <<"black knight greataxe">>, <<"黒騎士の大斧">>, 16, Requirements);
greataxe_lookup_test(67) ->
    Requirements = #{ strenght     => 50
                    , dexterity    => 8
                    , intelligence => 0
                    , faith        => 0},
    weapon_lookup_test(67, greataxe, <<"dragon king greataxe">>, <<"竜王の大斧">>, 24, Requirements).


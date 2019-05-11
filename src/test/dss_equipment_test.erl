-module(dss_equipment_test).

-include_lib("eunit/include/eunit.hrl").


-spec lookup_test() -> ok.
lookup_test() ->
     ok = dagger_lookup_test().


-spec list_test() -> ok.
list_test() ->
     ok = dagger_list_test().


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


-spec dagger_list_test() -> ok.
dagger_list_test() ->
    DaggerList = dss_equipment:list(dagger),
    6 = length(DaggerList),
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


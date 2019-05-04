-module(dss_skill_simulator).
-export_type([
    character/0
]).
-export([
    simulator_list/1
  , simulator/2

    % * Getters
  , class_id/1
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
  , rolling_levels/1
  , right_weapon1/1
  , right_weapon2/1
  , left_weapon1/1
  , left_weapon2/1
  , head_armor/1
  , chest_armor/1
  , hand_armor/1
  , leg_armor/1
  , ring/1
  , equip_weight/1
  , attunement_slots/1
]).

-opaque character() ::
    #{ classID         => pos_integer()
     , name            => #{ english  => unicode:unicode_binary()
                           , japanese => unicode:unicode_binary()}
     , levels          => pos_integer()
     , vitality        => pos_integer()
     , attunement      => pos_integer()
     , endurance       => pos_integer()
     , strength        => pos_integer()
     , dexterity       => pos_integer()
     , resistance      => pos_integer()
     , intelligence    => pos_integer()
     , faith           => pos_integer()
     , rightWeapon1    => dss_maybe:maybe(dss_equipment:equipment())
     , rightWeapon2    => dss_maybe:maybe(dss_equipment:equipment())
     , leftWeapon2     => dss_maybe:maybe(dss_equipment:equipment())
     , leftWeapon2     => dss_maybe:maybe(dss_equipment:equipment())
     , headArmor       => dss_maybe:maybe(pos_integer())
     , chestArmor      => dss_maybe:maybe(pos_integer())
     , handArmor       => dss_maybe:maybe(pos_integer())
     , legArmor        => dss_maybe:maybe(pos_integer())
     , ring            => dss_maybe:maybe(pos_integer())
     , equipWeight     => float()
     , attunementSlots => pos_integer()
    }.


-spec simulator_list([{unicode:unicode_binary(), unicode:unicode_binary()}]) -> [character()].
simulator_list(QsList) ->
    IDList = lists:seq(1, 10),
    [simulator(ClassID, QsList) || ClassID <- IDList].


-spec simulator(dss_classes:id(), [{unicode:unicode_binary(), unicode:unicode_binary()}]) -> character().
simulator(ClassID, QsList) ->
    Class = dss_classes:get(ClassID),
    EquipmentList = equipment_list(QsList),
    Character0 = make_character(Class, EquipmentList),
    LevelUped  = levels_up(Character0, QsList),
    SlotsPlus  = slots_plus_by_ring(LevelUped),
    calculate_equip_weight(SlotsPlus, QsList).


-spec equipment_list([{unicode:unicode_binary(), unicode:unicode_binary()}]) -> [dss_equipment:equipment()].
equipment_list(QsList) ->
    Params = [<<"right-weapon1">>, <<"right-weapon2">>, <<"left-weapon1">>,
              <<"left-weapon2">>, <<"head-armor">>, <<"chest-armor">>,
              <<"hand-armor">>, <<"leg-armor">>, <<"ring">>],
    equipment_list(QsList, Params, []).


-spec equipment_list(
        [{unicode:unicode_binary(), unicode:unicode_binary()}], [unicode:unicode_binary()], [dss_equipment:equipment()]
    ) -> [dss_equipment:equipment()].
equipment_list(_, [], EqpList) -> EqpList;

equipment_list(QsList, [Parameter | Tail], EqpList) ->
    Equipment = case lists:keyfind(Parameter, 1, QsList) of
        {Parameter, ID} ->
            case validate_id(ID) of
                none ->
                    none;
                IntID ->
                    case dss_equipment:lookup(
                        dss_equipment:equipment_type(IntID), IntID)
                    of
                        {value, Eqp} -> Eqp;
                        none         -> none
                    end
            end;
        false -> none
    end,
    equipment_list(QsList, Tail, lists:append(EqpList, [Equipment])).


-spec validate_id(unicode:unicode_binary()) -> pos_integer() | none.
validate_id(ID) ->
    case re:run(ID, <<"^\\d+$">>, [global, {capture, all, binary}]) of
        {match, [[Matched]]} -> binary_to_integer(Matched);
        nomatch -> none
    end.


-spec make_character(dss_classes:class(), [dss_equipment:equipment()]) -> character().
make_character(Class, EqpList) ->
    Params = [ rightWeapon1, rightWeapon2, leftWeapon1,
               leftWeapon2, headArmor, chestArmor,
               handArmor, legArmor, ring ],
    make_character(Class, EqpList, Params).


-spec make_character(dss_classes:class(), [dss_equipment:equipment()], [atom()]) -> character().
make_character(Class, [], []) -> Class;

make_character(Class, [Equipment | EqpTail], [Parameter | ParamTail]) ->
    make_character(maps:put(Parameter, Equipment, Class), EqpTail, ParamTail).


-spec levels_up(character(), [{unicode:unicode_binary(), unicode:unicode_binary()}]) -> character().
levels_up(Character, QsList) ->
    ParamList = [ <<"vitality">>, <<"attunement">>, <<"endurance">>, <<"strength">>,
                  <<"dexterity">>, <<"resistance">>, <<"intelligence">>, <<"faith">>, <<"attunement-slots">>],
    levels_up_(Character, QsList, ParamList).


-spec levels_up_(
        character(), [{unicode:unicode_binary(), unicode:unicode_binary()}], [unicode:unicode_binary()]
    ) -> character().
levels_up_(Character, _, []) -> Character;

levels_up_(Character, QsList, [<<"attunement-slots">> | Tail]) ->
    case lists:keyfind(<<"attunement-slots">>, 1, QsList) of
        {<<"attunement-slots">>, Value} ->
            QsSlots = case re:run(Value, <<"^[0-9]$|^10$">>, [global, {capture, all, binary}]) of
                {match, [[Matched]]} -> binary_to_integer(Matched);
                nomatch -> 0
            end,
            BaseAtt = maps:get(attunement, Character),
            BaseSlots = attunement_to_slots(BaseAtt),
            case QsSlots > BaseSlots of
                true ->
                    BaseLevels = maps:get(levels, Character),
                    QsAtt = requirement_attunement(QsSlots),
                    Diff  = QsAtt - BaseAtt,
                    Character1 = maps:put(attunement, BaseAtt + Diff, Character),
                    Character2 = maps:put(levels, BaseLevels + Diff, Character1),
                    maps:put(attunementSlots, QsSlots, Character2);
                false ->
                    maps:put(attunementSlots, BaseSlots, Character)
            end;
        false ->
            BaseSlots  = attunement_to_slots(attunement(Character)),
            Character1 = maps:put(attunementSlots, BaseSlots, Character),
            levels_up_(Character1, QsList, Tail)
    end;
levels_up_(Character, QsList, [Parameter | ParamTail]) ->
    case lists:keyfind(Parameter, 1, QsList) of
        {Parameter, Value} ->
            QsValue = case re:run(Value, <<"^[0-9]$|^[1-9][0-9]$">>, [global, {capture, all, binary}]) of
                {match, [[Matched]]} -> binary_to_integer(Matched);
                nomatch -> 0
            end,
            BaseValue = maps:get(binary_to_atom(Parameter, utf8), Character),
            Character1 = case QsValue - BaseValue of
                Diff when Diff > 0 ->
                    Character0 = maps:put(binary_to_atom(Parameter, utf8), BaseValue + Diff, Character),
                    BaseLevels = maps:get(levels, Character0),
                    maps:put(levels, BaseLevels + Diff, Character0);
                _ -> Character
            end,
            levels_up_(Character1, QsList, ParamTail);
        false -> levels_up_(Character, QsList, ParamTail)
    end.


-spec attunement_to_slots(pos_integer()) -> pos_integer().
attunement_to_slots(Att) when Att =< 9             -> 0;
attunement_to_slots(Att) when Att == 10; Att == 11 -> 1;
attunement_to_slots(Att) when Att == 12; Att == 13 -> 2;
attunement_to_slots(Att) when Att == 14; Att == 15 -> 3;
attunement_to_slots(Att) when 16 =< Att; Att =< 18 -> 4;
attunement_to_slots(Att) when 19 =< Att, Att =< 22 -> 5;
attunement_to_slots(Att) when 23 =< Att, Att =< 27 -> 6;
attunement_to_slots(Att) when 28 =< Att, Att =< 33 -> 7;
attunement_to_slots(Att) when 34 =< Att, Att =< 40 -> 8;
attunement_to_slots(Att) when 41 =< Att, Att =< 49 -> 9;
attunement_to_slots(Att) when 50 =< Att            -> 10.


-spec requirement_attunement(pos_integer()) -> pos_integer().
requirement_attunement(0)  -> 0;
requirement_attunement(1)  -> 10;
requirement_attunement(2)  -> 12;
requirement_attunement(3)  -> 14;
requirement_attunement(4)  -> 16;
requirement_attunement(5)  -> 19;
requirement_attunement(6)  -> 23;
requirement_attunement(7)  -> 28;
requirement_attunement(8)  -> 34;
requirement_attunement(9)  -> 41;
requirement_attunement(10) -> 50.


-spec slots_plus_by_ring(character()) -> character().
slots_plus_by_ring(Character) ->
    case ring(Character) of
        none -> Character;
        Ring -> case dss_equipment:id(Ring) of
                    % 白教の司祭の指輪 or 暗月の司祭の指輪
                    RingID when RingID == 435; RingID == 436 ->
                        Slots = attunement_slots(Character),
                        maps:put(attunementSlots, Slots + 1, Character);
                    _ -> Character
                end
    end.


-spec calculate_equip_weight(character(), [{unicode:unicode_binary(), unicode:unicode_binary()}]) -> character().
calculate_equip_weight(Character, QsList) ->
    List = [ rightWeapon1, rightWeapon2, leftWeapon1, leftWeapon2,
             headArmor, chestArmor, handArmor, legArmor ],
    Weight = sum_weight(Character, List, 0),
    Endurance = endurance(Character),
    CharEQW = case ring(Character) of
        none -> Endurance + 40;
        Ring -> case dss_equipment:equip_weight_magnification(Ring) of
                    none -> Endurance + 40;
                    Mag  -> (Endurance + 40) * Mag
                end
    end,
    Calculated = maps:put(equipWeight, Weight / CharEQW, Character),
    case lists:keyfind(<<"equipweight-levels">>, 1, QsList) of
        {<<"equipweight-levels">>, EWL} ->
            case re:run(EWL, <<"^[0-2]$">>, [global, {capture, all, binary}]) of
                 {match, [[Matched]]} -> equip_weight_levels(Calculated, CharEQW, binary_to_integer(Matched));
                 nomatch -> Calculated
            end;
        false ->
            Calculated
    end.


-spec sum_weight(character(), [atom()], pos_integer()) -> pos_integer().
sum_weight(Character, [], Sum) -> Sum;

sum_weight(Character, [Parameter | Tail], Sum) ->
    case maps:get(Parameter, Character) of
        none -> sum_weight(Character, Tail, Sum);
        Eqp  -> sum_weight(Character, Tail, dss_equipment:weight(Eqp) + Sum)
    end.


-spec equip_weight_levels(character(), integer() | float(), pos_integer()) -> character().
equip_weight_levels(Character, CharEQW, 0) ->
    Character;

equip_weight_levels(Character, CharEQW, 1) ->
    % 装備重量 25%(25%台はセーフ)
    equip_weight_levels(Character, CharEQW, 0.26);

equip_weight_levels(Character, CharEQW, 2) ->
    % 装備重量 50%(50%台はセーフ)
    equip_weight_levels(Character, CharEQW, 0.51);

equip_weight_levels(Character, CharEQW, WantEQW) ->
    case equip_weight(Character) of
        EQW when EQW > WantEQW ->
            Weight = EQW * CharEQW,
            UpedCharEQW = Weight / WantEQW,
            Ceiled = case math:ceil(UpedCharEQW) of
                Ceiled0 when Ceiled0 == UpedCharEQW ->
                    Ceiled0 + 1;
                Ceiled0 when Ceiled0 =/= UpedCharEQW ->
                    Ceiled0
            end,
            Diff = Ceiled - CharEQW,
            LevleUped = maps:put(levels, levels(Character) + Diff, Character),
            EnduranceUped = maps:put(endurance, endurance(Character) + Diff, LevleUped),
            maps:put(equipWeight, Weight / Ceiled, EnduranceUped);
        _ -> Character
    end.


-spec class_id(character()) -> dss_classes:id().
class_id(Character) -> maps:get(id, Character).


-spec name(character()) -> unicode:unicode_binary().
name(Character) -> maps:get(name, Character).


-spec levels(character()) -> pos_integer().
levels(Character) -> maps:get(levels, Character).


-spec vitality(character()) -> pos_integer().
vitality(Character) -> maps:get(vitality, Character).


-spec attunement(character()) -> pos_integer().
attunement(Character) -> maps:get(attunement, Character).


-spec endurance(character()) -> pos_integer().
endurance(Character) -> maps:get(endurance, Character).


-spec strength(character()) -> pos_integer().
strength(Character) -> maps:get(strength, Character).


-spec dexterity(character()) -> pos_integer().
dexterity(Character) -> maps:get(dexterity, Character).


-spec resistance(character()) -> pos_integer().
resistance(Character) -> maps:get(resistance, Character).


-spec intelligence(character()) -> pos_integer().
intelligence(Character) -> maps:get(intelligence, Character).


-spec faith(character()) -> pos_integer().
faith(Character) -> maps:get(faith, Character).


-spec rolling_levels(character()) -> 1 | 2 | 3 | 4.
rolling_levels(Character) -> maps:get(rollingLevels, Character).


-spec right_weapon1(character()) -> dss_maybe:maybe(dss_equipment:equipment()).
right_weapon1(Character) ->
    maps:get(rightWeapon1, Character).


-spec right_weapon2(character()) -> dss_maybe:maybe(dss_equipment:equipment()).
right_weapon2(Character) ->
    maps:get(rightWeapon2, Character).


-spec left_weapon1(character()) -> dss_maybe:maybe(dss_equipment:equipment()).
left_weapon1(Character) ->
    maps:get(leftWeapon1, Character).


-spec left_weapon2(character()) -> dss_maybe:maybe(dss_equipment:equipment()).
left_weapon2(Character) ->
    maps:get(leftWeapon2, Character).


-spec head_armor(character()) -> dss_maybe:maybe(dss_equipment:heas_armor()).
head_armor(Character) ->
    maps:get(headArmor, Character).


-spec chest_armor(character()) -> dss_maybe:maybe(dss_equipment:armor()).
chest_armor(Character) ->
    maps:get(chestArmor, Character).


-spec hand_armor(character()) -> dss_maybe:maybe(dss_equipment:armor()).
hand_armor(Character) ->
    maps:get(handArmor, Character).


-spec leg_armor(character()) -> dss_maybe:maybe(dss_equipment:armor()).
leg_armor(Character) ->
    maps:get(legArmor, Character).


-spec ring(character()) -> dss_maybe:maybe(dss_equipment:ring()).
ring(Character) ->
    maps:get(ring, Character).


-spec equip_weight(character()) -> float().
equip_weight(Character) ->
    maps:get(equipWeight, Character).


-spec attunement_slots(character()) -> pos_integer().
attunement_slots(Character) ->
    maps:get(attunementSlots, Character).


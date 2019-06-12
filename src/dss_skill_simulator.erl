-module(dss_skill_simulator).
-export_type([
    character/0
]).
-export([
    simulator_list/1
  , simulator/0
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
     , rightWeapon1    => dss_maybe:maybe(#{equipmentID => dss_equipment:id()})
     , rightWeapon2    => dss_maybe:maybe(#{equipmentID => dss_equipment:id()})
     , leftWeapon2     => dss_maybe:maybe(#{equipmentID => dss_equipment:id()})
     , leftWeapon2     => dss_maybe:maybe(#{equipmentID => dss_equipment:id()})
     , headArmor       => dss_maybe:maybe(#{equipmentID => dss_equipment:id()})
     , chestArmor      => dss_maybe:maybe(#{equipmentID => dss_equipment:id()})
     , handArmor       => dss_maybe:maybe(#{equipmentID => dss_equipment:id()})
     , legArmor        => dss_maybe:maybe(#{equipmentID => dss_equipment:id()})
     , ring            => dss_maybe:maybe(#{equipmentID => dss_equipment:id()})
     , equipWeight     => float()
     , attunementSlots => pos_integer()
    }.


-spec simulator_list([{unicode:unicode_binary(), unicode:unicode_binary()}]) -> [character()].
simulator_list(QsList) ->
    list_parallel(lists:seq(1, 10), QsList).


-spec list_parallel([pos_integer()], [{unicode:unicode_binary(), unicode:unicode_binary()}]) -> [character()].
list_parallel([], _) ->
    make_list([]);

list_parallel([ClassID | Tail], QsList) ->
    process_flag(trap_exit, true),
    {Pid, _Ref} = spawn_monitor(?MODULE, simulator, []),
    Pid ! {self(), ClassID, QsList},
    list_parallel(Tail, QsList).


-spec simulator() -> {pid(), pos_integer(), character()}.
simulator() ->
    receive
        {From, ClassID, QsList} ->
            Character = simulator(ClassID, QsList),
            From ! {self(), Character}
    after 10000 ->
        exit(timeout)
    end.


make_list(CharList) when length(CharList) == 10 -> CharList;

make_list(CharList) ->
    receive
        {_From, Character} ->
            make_list(lists:append(CharList, [Character]));

        {'DOWN', _Ref, process, _From, normal} ->
            %% 正常終了したプロセスを補足
            make_list(CharList);

        {'DOWN', _Ref, process, _From, timeout} ->
            error(timeout)
    end.


-spec simulator(dss_classes:id(), [{unicode:unicode_binary(), unicode:unicode_binary()}]) -> character().
simulator(ClassID, QsList) ->
    Class = dss_classes:get(ClassID),
    EqpIDList  = equipment_list(QsList),
    Character0 = make_character(Class, EqpIDList),
    LevelUped  = levels_up(Character0, QsList),
    SlotsPlus  = slots_plus_by_ring(LevelUped),
    Calculated = calculate_equip_weight(SlotsPlus, QsList),
    requirement(Calculated, QsList).


-spec equipment_list([{unicode:unicode_binary(), unicode:unicode_binary()}]) -> [dss_equipment:equipment()].
equipment_list(QsList) ->
    Params = [<<"right-weapon1">>, <<"right-weapon2">>, <<"left-weapon1">>,
              <<"left-weapon2">>, <<"head-armor">>, <<"chest-armor">>,
              <<"hand-armor">>, <<"leg-armor">>, <<"ring">>],
    equipment_list(QsList, Params, []).


-spec equipment_list(
        [{unicode:unicode_binary(), unicode:unicode_binary()}], [unicode:unicode_binary()], [dss_equipment:id()]
    ) -> [dss_equipment:equipment()].
equipment_list(_, [], EqpIDList) -> EqpIDList;

equipment_list(QsList, [Parameter | Tail], EqpIDList) ->
    EqpID = case lists:keyfind(Parameter, 1, QsList) of
        {Parameter, ID} ->
            case validate_id(ID) of
                none  -> none;
                IntID -> IntID
            end;
        false -> none
    end,
    equipment_list(QsList, Tail, lists:append(EqpIDList, [EqpID])).


-spec validate_id(unicode:unicode_binary()) -> pos_integer() | none.
validate_id(ID) ->
    case re:run(ID, <<"^\\d+$">>, [global, {capture, all, binary}]) of
        {match, [[Matched]]} -> binary_to_integer(Matched);
        nomatch -> none
    end.


-spec make_character(dss_classes:class(), [dss_maybe:maybe(dss_equipment:id())]) -> character().
make_character(Class, EqpIDList) ->
    Params = [ rightWeapon1, rightWeapon2, leftWeapon1,
               leftWeapon2, headArmor, chestArmor,
               handArmor, legArmor, ring ],
    make_character(Class, EqpIDList, Params).


-spec make_character(dss_classes:class(), [dss_maybe:maybe(dss_equipment:id())], [atom()]) -> character().
make_character(Class, [], []) -> Class;

make_character(Class, [EqpID | IDTail], [Parameter | ParamTail]) ->
    MaybeEqpID = case EqpID of
        none  -> none;
        EqpID -> {value, #{equipmentID => EqpID}}
    end,
    make_character(maps:put(Parameter, MaybeEqpID, Class), IDTail, ParamTail).


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
        {value, Ring} ->
            case maps:get(equipmentID, Ring) of
                % 白教の司祭の指輪 or 暗月の司祭の指輪
                RingID when RingID == 435; RingID == 436 ->
                    Slots = attunement_slots(Character),
                    maps:put(attunementSlots, Slots + 1, Character);
                _ -> Character
            end;
        none -> Character
    end.


-spec calculate_equip_weight(character(), [{unicode:unicode_binary(), unicode:unicode_binary()}]) -> character().
calculate_equip_weight(Character, QsList) ->
    List = [ rightWeapon1, rightWeapon2, leftWeapon1, leftWeapon2,
             headArmor, chestArmor, handArmor, legArmor ],
    Weight = sum_weight(Character, List, 0),
    Endurance = endurance(Character),
    CharEQW = case ring(Character) of
        {value, RingIDMap} ->
            HeadMag = case head_armor(Character) of
                {value, HeadIDMap} ->
                    HeadID = maps:get(equipmentID, HeadIDMap),
                    HeadArmor = dss_equipment:get(head_armor, HeadID),
                    case dss_equipment:equip_weight_magnification(HeadArmor) of
                        {value, Mag} -> Mag;
                        none -> 1
                    end;
                none -> 1
            end,
            RingID = maps:get(equipmentID, RingIDMap),
            Ring   = dss_equipment:get(ring, RingID),
            case dss_equipment:equip_weight_magnification(Ring) of
                {value, RingMag} -> (Endurance + 40) * HeadMag * RingMag;
                none             -> (Endurance + 40) * HeadMag
            end;
        none -> Endurance + 40
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
sum_weight(_, [], Sum) -> Sum;

sum_weight(Character, [Parameter | Tail], Sum) ->
    case maps:get(Parameter, Character) of
        {value, EqpIDMap} ->
            EqpID = maps:get(equipmentID, EqpIDMap),
            Equipment = dss_equipment:get(dss_equipment:equipment_type(EqpID), EqpID),
            sum_weight(Character, Tail, dss_equipment:weight(Equipment) + Sum);
        none ->
            sum_weight(Character, Tail, Sum)
    end.


-spec equip_weight_levels(character(), integer() | float(), pos_integer()) -> character().
equip_weight_levels(Character, _, 0) ->
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
            LevelUped = maps:put(levels, round(levels(Character)+Diff), Character),
            UpedEndurance = maps:put(endurance, round(endurance(Character)+Diff), LevelUped),
            maps:put(equipWeight, Weight / (maps:get(endurance, UpedEndurance)+40), UpedEndurance);
        _ -> Character
    end.


-spec requirement(character(), [{unicode:unicode_binary(), unicode:unicode_binary()}]) -> character().
requirement(Character, QsList) ->
    List = [ rightWeapon1, rightWeapon2, leftWeapon1, leftWeapon2 ],
    StrengthUped = strength_up(Character, List, QsList),
    status_up(StrengthUped, List).


-spec strength_up(character(), [{unicode:unicode_binary(), unicode:unicode_binary()}], [atom()]) -> character().
strength_up(Character, ParamList, QsList) ->
    RequireStrList = requirement_strength(Character, ParamList),
    BaseStr = maps:get(strength, Character),
    case lists:max(RequireStrList) of
        RequireStr when RequireStr > BaseStr ->
            case lists:keyfind(<<"hold">>, 1, QsList) of
                {<<"hold">>, Hold} ->
                    HoldVal = case re:run(Hold, <<"^[0-2]$">>, [global, {capture, all, binary}]) of
                        {match, [[Matched]]} -> binary_to_integer(Matched);
                        nomatch -> 0
                    end,
                    Magnification = case HoldVal of
                        2    -> 1.5;
                        _    -> 1
                    end,
                    WantStr = RequireStr / Magnification,
                    Diff = math:ceil(WantStr) - BaseStr,
                    case Diff of
                        Diff when Diff =< 0 ->
                            Character;
                        Diff when Diff > 0 ->
                            StrUped = maps:put(strength, BaseStr + Diff, Character),
                            maps:put(levels, levels(Character) + Diff, StrUped)
                    end;
                false ->
                    Character
            end;
        RequireStr when RequireStr =< BaseStr ->
            Character
    end.


-spec requirement_strength(character(), [atom()]) -> [pos_integer()].
requirement_strength(Character, ParamList) ->
    requirement_strength(Character, ParamList, []).


-spec requirement_strength(character(), [atom()], [pos_integer()]) -> [pos_integer()].
requirement_strength(_, [], RequireStrList) ->
    RequireStrList;

requirement_strength(Character, [Param | Tail], RequireStrList) ->
    case maps:get(Param, Character) of
        {value, EqpIDMap} ->
            EqpID = maps:get(equipmentID, EqpIDMap),
            Eqp = dss_equipment:get(dss_equipment:equipment_type(EqpID), EqpID),
            EquipStr = maps:get(strength, maps:get(requirements, Eqp)),
            requirement_strength(Character, Tail, lists:append(RequireStrList, [EquipStr]));
        none ->
            requirement_strength(Character, Tail, lists:append(RequireStrList, [0]))
    end.


-spec status_up(character(), [atom()]) -> character().
status_up(Character, []) ->
    Character;

status_up(Character, [Parameter | Tail]) ->
    case maps:get(Parameter, Character) of
        {value, EqpIDMap} ->
            Character1 = status_up_(Character, maps:get(equipmentID, EqpIDMap)),
            status_up(Character1, Tail);
        none ->
            status_up(Character, Tail)
    end.


-spec status_up_(character(), pos_integer()) -> character().
status_up_(Character, EqpID) ->
    List = [ dexterity, intelligence, faith ],
    status_up_(Character, EqpID, List).

-spec status_up_(character(), pos_integer(), [atom()]) -> character().
status_up_(Character, _, []) -> Character;

status_up_(Character, EqpID, [Parameter | Tail]) ->
    Eqp = dss_equipment:get(dss_equipment:equipment_type(EqpID), EqpID),
    Require = dss_equipment:requirements(Eqp),
    BaseValue = maps:get(Parameter, Character),
    Character1 = case maps:get(Parameter, Require) of
        RequireVal when RequireVal > BaseValue ->
            Diff = RequireVal - BaseValue,
            StrUped = maps:put(Parameter, BaseValue + Diff, Character),
            maps:put(levels, levels(Character) + Diff, StrUped);
        RequireVal when RequireVal =< BaseValue ->
            Character
    end,
    status_up_(Character1, EqpID, Tail).


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


-spec right_weapon1(character()) -> dss_maybe:maybe(dss_equipment:id()).
right_weapon1(Character) ->
    maps:get(rightWeapon1, Character).


-spec right_weapon2(character()) -> dss_maybe:maybe(dss_equipment:id()).
right_weapon2(Character) ->
    maps:get(rightWeapon2, Character).


-spec left_weapon1(character()) -> dss_maybe:maybe(dss_equipment:id()).
left_weapon1(Character) ->
    maps:get(leftWeapon1, Character).


-spec left_weapon2(character()) -> dss_maybe:maybe(dss_equipment:id()).
left_weapon2(Character) ->
    maps:get(leftWeapon2, Character).


-spec head_armor(character()) -> dss_maybe:maybe(dss_equipment:id()).
head_armor(Character) ->
    maps:get(headArmor, Character).


-spec chest_armor(character()) -> dss_maybe:maybe(dss_equipment:id()).
chest_armor(Character) ->
    maps:get(chestArmor, Character).


-spec hand_armor(character()) -> dss_maybe:maybe(dss_equipment:id()).
hand_armor(Character) ->
    maps:get(handArmor, Character).


-spec leg_armor(character()) -> dss_maybe:maybe(dss_equipment:id()).
leg_armor(Character) ->
    maps:get(legArmor, Character).


-spec ring(character()) -> dss_maybe:maybe(dss_equipment:id()).
ring(Character) ->
    maps:get(ring, Character).


-spec equip_weight(character()) -> float().
equip_weight(Character) ->
    maps:get(equipWeight, Character).


-spec attunement_slots(character()) -> pos_integer().
attunement_slots(Character) ->
    maps:get(attunementSlots, Character).


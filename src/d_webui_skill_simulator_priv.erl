-module(d_webui_skill_simulator_priv).
-behavior(cowboy_rest).
-export([
    init/2
  , allowed_methods/2
  , resource_exists/2
  , content_types_provided/2
  , element_to_list/2
  , element_to_JSON/2
]).

-include("dss_error.hrl").

-type resource() :: atom().
-type state()    :: resource()
                 | {resource(), dss_skill_simulator:class()}
                 | {resource(), dss_skill_simulator:id()}
                 .


-spec init(cowboy_req:req(), state()) -> {resource(), cowboy_req:req(), state()}.
init(Req, State) ->
    QS = cowboy_req:parse_qs(Req),
    case State of
        collection -> {cowboy_rest, Req, {inited_collection, QS}};
        element    -> {cowboy_rest, Req, {inited_element, cowboy_req:binding(classID, Req), QS}}
    end.


-spec allowed_methods(cowboy_req:req(), state()) -> {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>], Req, State}.


-spec resource_exists(cowboy_req:req(), state()) -> {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, {provided_collection, QS}) ->
    {true, Req, {existed_collection, dss_skill_simulator:simulator_list(QS)}};

resource_exists(Req, {provided_element, ClassID, QS}) ->
    {true, Req, {existed_element, dss_skill_simulator:simulator(ClassID, QS)}}.


-spec content_types_provided(
        cowboy_req:req(), state()
    ) -> {[{binary() | {binary(), binary(), '*'
            | [{binary(), binary()}]}, atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, {inited_collection, QS}) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_list}], Req, {provided_collection, QS}};

content_types_provided(Req, {inited_element, ClassID, QS}) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_JSON}], Req, {provided_element, ClassID, QS}}.


-spec element_to_list(cowboy_req:req(), state()) -> {[dss_equipment:equipment()], cowboy_req:req(), state()}.
element_to_list(Req, State={existed_collection, List}) ->
    Characters = [element_to_json_value(Character) || Character <- List],
    {ok, JSON} = jsone_encode:encode(Characters),
    {JSON, Req, State}.


-spec element_to_JSON(cowboy_req:req(), state()) -> {dss_skill_simulator:character(), cowboy_req:req(), state()}.
element_to_JSON(Req, State={existed_element, Character}) ->
    {ok, JSON} = jsone_encode:encode(
        element_to_json_value(Character)),
    {JSON, Req, State}.


-spec element_to_json_value(dss_skill_simulator:character()) -> jsone:json_value().
element_to_json_value(Character) ->
    {[
        {<<"classID">>     , dss_skill_simulator:class_id(Character)}
      , {<<"name">>        , dss_skill_simulator:name(Character)}
      , {<<"levels">>      , dss_skill_simulator:levels(Character)}
      , {<<"vitality">>    , dss_skill_simulator:vitality(Character)}
      , {<<"attunement">>  , dss_skill_simulator:attunement(Character)}
      , {<<"endurance">>   , dss_skill_simulator:endurance(Character)}
      , {<<"strength">>    , dss_skill_simulator:strength(Character)}
      , {<<"dexterity">>   , dss_skill_simulator:dexterity(Character)}
      , {<<"resistance">>  , dss_skill_simulator:resistance(Character)}
      , {<<"intelligence">>, dss_skill_simulator:intelligence(Character)}
      , {<<"faith">>       , dss_skill_simulator:faith(Character)}
      , {<<"rightWeapon1">>,
            case dss_skill_simulator:right_weapon1(Character) of
                none -> null;
                RW1  -> RW1
            end}
      , {<<"rightWeapon2">>,
            case dss_skill_simulator:right_weapon2(Character) of
                none -> null;
                RW2  -> RW2
            end}
      , {<<"leftWeapon1">>,
            case dss_skill_simulator:left_weapon1(Character) of
                none -> null;
                LW1  -> LW1
            end}
      , {<<"leftWeapon2">>,
            case dss_skill_simulator:left_weapon2(Character) of
                none -> null;
                LW2  -> LW2
            end}
      , {<<"headArmor">>,
            case dss_skill_simulator:head_armor(Character) of
                none -> null;
                HA   -> HA
            end}
      , {<<"chestArmor">>,
            case dss_skill_simulator:chest_armor(Character) of
                none -> null;
                CA   -> CA
            end}
      , {<<"handArmor">>,
            case dss_skill_simulator:hand_armor(Character) of
                none -> null;
                HaA  -> HaA
            end}
      , {<<"legArmor">>,
            case dss_skill_simulator:leg_armor(Character) of
                none -> null;
                LA   -> LA
            end}
      , {<<"ring">>, case dss_skill_simulator:ring(Character) of
            none -> null;
            Ring -> Ring
        end}
      , {<<"equipWeight">>, dss_skill_simulator:equip_weight(Character)}
      , {<<"attunementSlots">>, dss_skill_simulator:attunement_slots(Character)}
    ]}.


-module(d_webui_equipment_priv).
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
                 | {resource(), dss_equipment:equipment_type(), dss_equipment:detail()}
                 | {resource(), dss_equipment:equipment_type(), dss_equipment:detail(), dss_equipment:id()}
                 | {resource(), dss_equipment:equipment(), dss_equipment:equipment_type(), dss_equipment:detail()}
                 | {resource(), [dss_equipment:equipment()], dss_equipment:equipment_type(), dss_equipment:detail()}
                 .


-spec init(cowboy_req:req(), state()) -> {resource(), cowboy_req:req(), state()}.
init(Req, State) ->
    case State of
        collection -> {cowboy_rest, Req,
                            { inited_collection
                            , cowboy_req:binding(equipmentType, Req)}};
        element    -> {cowboy_rest, Req,
                            { inited_element
                            , cowboy_req:binding(equipmentType, Req)
                            , cowboy_req:binding(equipmentID, Req)}}
    end.


-spec allowed_methods(cowboy_req:req(), state()) -> {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>], Req, State}.


-spec resource_exists(cowboy_req:req(), state()) -> {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, {provided_collection, EqpType}) ->
    {true, Req, {existed_collection, dss_equipment:list(EqpType), EqpType}};

resource_exists(Req, State={provided_element, EqpType, ID}) ->
    case dss_equipment:lookup(EqpType, ID) of
        {value, Equipment} ->
            {true, Req, {existed_element, Equipment, EqpType}};
        none ->
            E    = ?DSS_NOT_FOUND,
            Body = dss_error:error_to_JSON(E),
            Req2 = cowboy_req:set_resp_body(Body, Req),
            {false, Req2, State}
    end.


-spec content_types_provided(
        cowboy_req:req(), state()
    ) -> {[{binary() | {binary(), binary(), '*'
            | [{binary(), binary()}]}, atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, {inited_collection, EqpType}) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_list}], Req, {provided_collection, EqpType}};

content_types_provided(Req, {inited_element, EqpType, ID}) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_JSON}], Req, {provided_element, EqpType, ID}}.


-spec element_to_list(cowboy_req:req(), state()) -> {[dss_material:material()], cowboy_req:req(), state()}.
element_to_list(Req, State={existed_collection, List, EqpType}) ->
    Equipments = [element_to_json_value(Equipment, EqpType) || Equipment <- List],
    {ok, JSON} = jsone_encode:encode(Equipments),
    {JSON, Req, State}.


-spec element_to_JSON(cowboy_req:req(), state()) -> {dss_equipment:equipment(), cowboy_req:req(), state()}.
element_to_JSON(Req, State={existed_element, Equipment, EqpType}) ->
    {ok, JSON} = jsone_encode:encode(
        element_to_json_value(Equipment, EqpType)),
    {JSON, Req, State}.


-spec element_to_json_value(
        dss_equipment:ring()
      , {dss_equipment:equipment_type()
      , dss_equipment:detail()}
    ) -> jsone:json_value().
element_to_json_value(Ring, ring) ->
    EquipWeight = case dss_equipment:equip_weight(Ring) of
        none -> null;
        EW   -> EW
    end,
    AttunementSlots = case dss_equipment:attunement_slots(Ring) of
        none -> null;
        AS   -> AS
    end,
    {[
        {<<"id">>             , dss_equipment:id(Ring)}
      , {<<"name">>           , dss_equipment:name(Ring)}
      , {<<"effects">>        , dss_equipment:effects(Ring)}
      , {<<"equipWeight">>    , EquipWeight}
      , {<<"attunementSlots">>, AttunementSlots}
    ]};
element_to_json_value(HeadArmor, head_armor) ->
    EquipWeight = case dss_equipment:equip_weight(HeadArmor) of
        none -> null;
        EW   -> EW
    end,
    {[
        {<<"id">>         , dss_equipment:id(HeadArmor)}
      , {<<"name">>       , dss_equipment:name(HeadArmor)}
      , {<<"weight">>     , dss_equipment:weight(HeadArmor)}
      , {<<"effects">>    , dss_equipment:effects(HeadArmor)}
      , {<<"equipWeight">>, EquipWeight}
    ]};
element_to_json_value(Equipment, EqpType)
    when EqpType == chest_armor; EqpType == hand_armor; EqpType == leg_armor ->
    {[
        {<<"id">>    , dss_equipment:id(Equipment)}
      , {<<"name">>  , dss_equipment:name(Equipment)}
      , {<<"weight">>, dss_equipment:weight(Equipment)}
    ]};
element_to_json_value(Equipment, _) ->
    {[
        {<<"id">>          , dss_equipment:id(Equipment)}
      , {<<"name">>        , dss_equipment:name(Equipment)}
      , {<<"weight">>      , dss_equipment:weight(Equipment)}
      , {<<"requirements">>, dss_equipment:requirements(Equipment)}
    ]}.


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
        collection_detail -> {cowboy_rest, Req,
                                    { inited_collection_detail
                                    , cowboy_req:binding(equipmentType, Req)
                                    , cowboy_req:binding(detail, Req)}};
        element_detail    -> {cowboy_rest, Req,
                                    { inited_element_detail
                                    , cowboy_req:binding(equipmentType, Req)
                                    , cowboy_req:binding(detail, Req)
                                    , cowboy_req:binding(id, Req)}}
    end.


-spec allowed_methods(cowboy_req:req(), state()) -> {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>], Req, State}.


-spec resource_exists(cowboy_req:req(), state()) -> {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, {provided_collection_detail, EqpType, Detail}) ->
    {true,  Req, {existed_collection_detail, dss_equipment:list({EqpType, Detail}), EqpType, Detail}};

resource_exists(Req, State={provided_element_detail, EqpType, Detail, ID}) ->
    case dss_equipment:lookup({EqpType, Detail}, ID) of
        {value, Equipment} ->
            {true, Req, {existed_element_detail, Equipment, EqpType, Detail}};
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
content_types_provided(Req, {inited_collection_detail, EqpType, Detail}) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_list}], Req, {provided_collection_detail, EqpType, Detail}};

content_types_provided(Req, {inited_element_detail, EqpType, Detail, ID}) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_JSON}], Req, {provided_element_detail, EqpType, Detail, ID}}.


-spec element_to_list(cowboy_req:req(), state()) -> {[dss_material:material()], cowboy_req:req(), state()}.
element_to_list(Req, State={existed_collection_detail, List, EqpType, Detail}) ->
    Equipments = [element_to_json_value(Equipment, {EqpType, Detail}) || Equipment <- List],
    {ok, JSON} = jsone_encode:encode(Equipments),
    {JSON, Req, State}.


-spec element_to_JSON(cowboy_req:req(), state()) -> {dss_equipment:equipment(), cowboy_req:req(), state()}.
element_to_JSON(Req, State={existed_element_detail, Equipment, EqpType, Detail}) ->
    {ok, JSON} = jsone_encode:encode(
        element_to_json_value(Equipment, {EqpType, Detail})),
    {JSON, Req, State}.


-spec element_to_json_value(
        dss_equipment:equipment()
      , {dss_equipment:equipment_type()
      , dss_equipment:detail()}
    ) -> jsone:json_value().
element_to_json_value(Equipment, {_, Detail})
    when Detail == dagger; Detail == straight_sword; Detail == greats_sword;
         Detail == ultra_greatsword; Detail == curved_sword; Detail == curved_greatsword;
         Detail == katana; Detail == thrusting_sword; Detail == axe; Detail == greataxe;
         Detail == hammer; Detail == great_hammer; Detail == spear; Detail == halberd;
         Detail == whip; Detail == fist; Detail == bow; Detail == crossbow;
         Detail == catalyst; Detail == talisman; Detail == pyromancy_flame;
         Detail == small_shield; Detail == normal_shield; Detail == large_shield ->
    {[
        {<<"id">>          , dss_equipment:id(Equipment)},
        {<<"name">>        , dss_equipment:name(Equipment)},
        {<<"weight">>      , dss_equipment:weight(Equipment)},
        {<<"requirements">>, dss_equipment:requirements(Equipment)}
    ]}.


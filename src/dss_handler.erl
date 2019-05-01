-module(dss_handler).
-export([
    id/2
  , equipment_type/2
  , detail/2
  , handler/0
]).


-spec id(forward | reverse, pos_integer())
    -> {ok, dss_material:id()} | {error, atom()}.
id(forward, ID) ->
    case re:run(ID, <<"^[1-9]$|^[1-5][0-9]$|^6[0-7]$">>, [global]) of
        {match, _} -> {ok, binary_to_integer(ID)};
        nomatch    -> {error, not_a_id}
    end;
id(forward, _)  -> {error, not_a_id};
id(reverse, ID) -> {ok, ID}.


-spec equipment_type(forward | reverse, unicode:unicode_binary())
    -> {ok, dss_equipment:equipment_type()} | {error, atom()}.
equipment_type(forward, EqpType)
    when EqpType == <<"weapon">>;
         EqpType == <<"shield">> ->
    {ok, binary_to_atom(EqpType, utf8)};
equipment_type(forward, _) -> {error, not_a_equipment_type};
equipment_type(reverse, EqpType) -> {ok, EqpType}.


-spec detail(forward | reverse, unicode:unicode_binary())
    -> {ok, dss_equipment:equipment_type()} | {error, atom()}.
detail(forward, <<"dagger">>)         -> {ok, dagger};
detail(forward, <<"straight-sword">>) -> {ok, straight_sword};
detail(forward, <<"small-shield">>)   -> {ok, small_shield};
detail(forward, _) -> {error, not_a_detail};
detail(reverse, Detail) -> {ok, Detail}.


-spec handler() -> cowboy_router:dispatch_rules().
handler() ->
    cowboy_router:compile([
        {'_', [
            {"/dss/sample", d_webui_sample, element}
          , {"/v1/character/classes", d_webui_classes_priv, collection}
          , {"/v1/character/classes/:classID"
            , [ {classID, fun id/2} ]
            , d_webui_classes_priv, element}
          , {"/v1/equipment/:equipmentType/:detail"
            , [ {equipmentType, fun equipment_type/2}
              , {detail, fun detail/2} ]
            , d_webui_equipment_priv, collection_detail}
          , {"/v1/equipment/:equipmentType/:detail/:id"
            , [ {equipmentType, fun equipment_type/2}
              , {detail, fun detail/2}
              , {id, fun id/2} ]
            , d_webui_equipment_priv, element_detail}
        ]}
    ]).


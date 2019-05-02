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
equipment_type(forward, <<"weapon">>)      -> {ok, weapon};
equipment_type(forward, <<"shield">>)      -> {ok, shield};
equipment_type(forward, <<"head-armor">>)  -> {ok, head_armor};
equipment_type(forward, <<"chest-armor">>) -> {ok, chest_armor};
equipment_type(forward, <<"hand-armor">>)  -> {ok, hand_armor};
equipment_type(forward, <<"leg-armor">>)   -> {ok, leg_armor};
equipment_type(forward, <<"ring">>)        -> {ok, ring};
equipment_type(forward, _) -> {error, not_a_equipment_type};
equipment_type(reverse, EqpType) -> {ok, EqpType}.


-spec detail(forward | reverse, unicode:unicode_binary())
    -> {ok, dss_equipment:equipment_type()} | {error, atom()}.
detail(forward, <<"dagger">>)            -> {ok, dagger};
detail(forward, <<"straight-sword">>)    -> {ok, straight_sword};
detail(forward, <<"greats-sword">>)      -> {ok, greats_sword};
detail(forward, <<"ultra-greatsword">>)  -> {ok, ultra_greatsword};
detail(forward, <<"curved-sword">>)      -> {ok, curved_sword};
detail(forward, <<"curved-greatsword">>) -> {ok, curved_greatsword};
detail(forward, <<"katana">>)            -> {ok, katana};
detail(forward, <<"thrusting-sword">>)   -> {ok, thrusting_sword};
detail(forward, <<"axe">>)               -> {ok, axe};
detail(forward, <<"greataxe">>)          -> {ok, greataxe};
detail(forward, <<"hammer">>)            -> {ok, hammer};
detail(forward, <<"great-hammer">>)      -> {ok, great_hammer};
detail(forward, <<"spear">>)             -> {ok, spear};
detail(forward, <<"halberd">>)           -> {ok, halberd};
detail(forward, <<"whip">>)              -> {ok, whip};
detail(forward, <<"fist">>)              -> {ok, fist};
detail(forward, <<"bow">>)               -> {ok, bow};
detail(forward, <<"crossbow">>)          -> {ok, crossbow};
detail(forward, <<"catalyst">>)          -> {ok, catalyst};
detail(forward, <<"talisman">>)          -> {ok, talisman};
detail(forward, <<"pyromancy-flame">>)   -> {ok, pyromancy_flame};
detail(forward, <<"small-shield">>)      -> {ok, small_shield};
detail(forward, <<"normal-shield">>)     -> {ok, normal_shield};
detail(forward, <<"large-shield">>)      -> {ok, large_shield};
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
          , {"/v1/equipment/:equipmentType"
            , [{equipmentType, fun equipment_type/2}]
            , d_webui_equipment_priv, collection}
          , {"/v1/equipment/:equipmentType/:id"
            , [{equipmentType, fun equipment_type/2}
              , {id, fun id/2}]
            , d_webui_equipment_priv, element}
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


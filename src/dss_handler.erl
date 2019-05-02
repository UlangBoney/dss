-module(dss_handler).
-export([
    class_id/2
  , equipment_id/2
  , equipment_type/2
  , handler/0
]).


-spec class_id(forward | reverse, pos_integer())
    -> {ok, dss_classes:id()} | {error, atom()}.
class_id(forward, ID) ->
    case re:run(ID, <<"^[1-9]$|^10$">>, [global]) of
        {match, _} -> {ok, binary_to_integer(ID)};
        nomatch    -> {error, not_a_id}
    end;
class_id(forward, _)  -> {error, not_a_id};
class_id(reverse, ID) -> {ok, ID}.


-spec equipment_id(forward | reverse, pos_integer())
    -> {ok, dss_equipment:id()} | {error, atom()}.
equipment_id(forward, ID) ->
    case re:run(ID, <<"^[1-9]$|^[1-5][0-9]$|^6[0-7]$">>, [global]) of
        {match, _} -> {ok, binary_to_integer(ID)};
        nomatch    -> {error, not_a_id}
    end;
equipment_id(forward, _)  -> {error, not_a_id};
equipment_id(reverse, ID) -> {ok, ID}.


-spec equipment_type(forward | reverse, unicode:unicode_binary())
    -> {ok, dss_equipment:equipment_type()} | {error, atom()}.
equipment_type(forward, <<"dagger">>)            -> {ok, dagger};
equipment_type(forward, <<"straight-sword">>)    -> {ok, straight_sword};
equipment_type(forward, <<"greats-sword">>)      -> {ok, greats_sword};
equipment_type(forward, <<"ultra-greatsword">>)  -> {ok, ultra_greatsword};
equipment_type(forward, <<"curved-sword">>)      -> {ok, curved_sword};
equipment_type(forward, <<"curved-greatsword">>) -> {ok, curved_greatsword};
equipment_type(forward, <<"katana">>)            -> {ok, katana};
equipment_type(forward, <<"thrusting-sword">>)   -> {ok, thrusting_sword};
equipment_type(forward, <<"axe">>)               -> {ok, axe};
equipment_type(forward, <<"greataxe">>)          -> {ok, greataxe};
equipment_type(forward, <<"hammer">>)            -> {ok, hammer};
equipment_type(forward, <<"great-hammer">>)      -> {ok, great_hammer};
equipment_type(forward, <<"spear">>)             -> {ok, spear};
equipment_type(forward, <<"halberd">>)           -> {ok, halberd};
equipment_type(forward, <<"whip">>)              -> {ok, whip};
equipment_type(forward, <<"fist">>)              -> {ok, fist};
equipment_type(forward, <<"bow">>)               -> {ok, bow};
equipment_type(forward, <<"crossbow">>)          -> {ok, crossbow};
equipment_type(forward, <<"catalyst">>)          -> {ok, catalyst};
equipment_type(forward, <<"talisman">>)          -> {ok, talisman};
equipment_type(forward, <<"pyromancy-flame">>)   -> {ok, pyromancy_flame};
equipment_type(forward, <<"small-shield">>)      -> {ok, small_shield};
equipment_type(forward, <<"normal-shield">>)     -> {ok, normal_shield};
equipment_type(forward, <<"large-shield">>)      -> {ok, large_shield};
equipment_type(forward, <<"head-armor">>)        -> {ok, head_armor};
equipment_type(forward, <<"chest-armor">>)       -> {ok, chest_armor};
equipment_type(forward, <<"hand-armor">>)        -> {ok, hand_armor};
equipment_type(forward, <<"leg-armor">>)         -> {ok, leg_armor};
equipment_type(forward, <<"ring">>)              -> {ok, ring};
equipment_type(forward, _) -> {error, not_a_detail};
equipment_type(reverse, Detail) -> {ok, Detail}.


-spec handler() -> cowboy_router:dispatch_rules().
handler() ->
    cowboy_router:compile([
        {'_', [
            {"/dss/sample", d_webui_sample, element}
          , {"/v1/character/classes", d_webui_classes_priv, collection}
          , {"/v1/character/classes/:classID"
            , [ {classID, fun class_id/2} ]
            , d_webui_classes_priv, element}
          , {"/v1/equipment/:equipmentType"
            , [{equipmentType, fun equipment_type/2}]
            , d_webui_equipment_priv, collection}
          , {"/v1/equipment/:equipmentType/:equipmentID"
            , [{equipmentType, fun equipment_type/2}
              ,{equipmentID, fun equipment_id/2}]
            , d_webui_equipment_priv, element}
        ]}
    ]).


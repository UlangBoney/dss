-module(dss_handler).
-export([
    class_id/2
  , dagger_id/2
  , handler/0
]).


-spec class_id(forward | reverse, unicode:unicode_binary())
    -> {ok, dss_material:id()} | {error, atom()}.
class_id(forward, ID) ->
    case re:run(ID, <<"^[1-9]$|^10$">>, [global]) of
        {match, _} -> {ok, binary_to_integer(ID)};
        nomatch    -> {error, not_a_id}
    end;
class_id(forward, _)  -> {error, not_a_id};
class_id(reverse, ID) -> {ok, ID}.


-spec dagger_id(forward | reverse, unicode:unicode_binary())
    -> {ok, dss_material:id()} | {error, atom()}.
dagger_id(forward, ID) ->
    case re:run(ID, <<"^[1-6]$">>, [global]) of
        {match, _} -> {ok, binary_to_integer(ID)};
        nomatch    -> {error, not_a_id}
    end;
dagger_id(forward, _)  -> {error, not_a_id};
dagger_id(reverse, ID) -> {ok, ID}.


-spec straight_sword_id(forward | reverse, unicode:unicode_binary())
    -> {ok, dss_material:id()} | {error, atom()}.
straight_sword_id(forward, ID) ->
    case re:run(ID, <<"^[1-9]$|^[1][0-3]$">>, [global]) of
        {match, _} -> {ok, binary_to_integer(ID)};
        nomatch    -> {error, not_a_id}
    end;
straight_sword_id(forward, _)  -> {error, not_a_id};
straight_sword_id(reverse, ID) -> {ok, ID}.


-spec handler() -> cowboy_router:dispatch_rules().
handler() ->
    cowboy_router:compile([
        {'_', [
            {"/dss/sample", d_webui_sample, element}
          , {"/v1/character/classes", d_webui_material_priv, collection_classes}
          , {"/v1/character/classes/:classID"
            , [{classID, fun class_id/2}]
            , d_webui_material_priv, element_classes}
          , {"/v1/equipment/weapons/daggers", d_webui_material_priv, collection_daggers}
          , {"/v1/equipment/weapons/daggers/:daggerID"
            , [{daggerID, fun dagger_id/2}]
            , d_webui_material_priv, element_daggers}
          , {"/v1/equipment/weapons/straight-swords", d_webui_material_priv, collection_straight_swords}
          , {"/v1/equipment/weapons/straight-swords/:straightSwordID"
            , [{straightSwordID, fun straight_sword_id/2}]
            , d_webui_material_priv, element_straight_swords}
        ]}
    ]).


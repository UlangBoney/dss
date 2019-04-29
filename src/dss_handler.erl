-module(dss_handler).
-export([
    id/2
  , handler/0
]).


-spec id(forward | reverse, unicode:unicode_binary())
    -> {ok, bson:objectid()} | {error, atom()}.
id(forward, ID) ->
    case re:run(ID, <<"^[1-9]$|^10$">>, [global]) of
        {match, _} -> {ok, binary_to_integer(ID)};
        nomatch    -> {error, not_a_id}
    end;
id(forward, _)  -> {error, not_a_id};
id(reverse, ID) -> {ok, ID}.


-spec handler() -> cowboy_router:dispatch_rules().
handler() ->
    cowboy_router:compile([
        {'_', [
            {"/dss/sample", d_webui_sample, element}
          , {"/v1/classes/:classID"
            , [{classID, fun id/2}]
            , d_webui_classes_priv, element}
        ]}
    ]).


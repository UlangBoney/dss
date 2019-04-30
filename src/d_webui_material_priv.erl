-module(d_webui_material_priv).
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
                 | {resource(), dss_material:material()}
                 | {resource(), dss_material:id()}
                 .


-spec init(cowboy_req:req(), state()) -> {resource(), cowboy_req:req(), state()}.
init(Req, State) ->
    case State of
        element_classes -> {cowboy_rest, Req, {element_classes, cowboy_req:binding(classID, Req)}};
        element_daggers -> {cowboy_rest, Req, {element_daggers, cowboy_req:binding(daggerID, Req)}};
        element_straight_swords ->
            {cowboy_rest, Req, {element_straight_swords, cowboy_req:binding(straightSwordID, Req)}};
        _               -> {cowboy_rest, Req, State}
    end.


-spec allowed_methods(cowboy_req:req(), state()) -> {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>], Req, State}.


-spec resource_exists(cowboy_req:req(), state()) -> {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State=collection_classes) ->
    {true,  Req, {State, dss_material:list(classes)}};
resource_exists(Req, State=collection_daggers) ->
    {true,  Req, {State, dss_material:list(daggers)}};
resource_exists(Req, State=collection_straight_swords) ->
    {true,  Req, {State, dss_material:list(straight_swords)}};

resource_exists(Req, State={element_classes, _}) ->
    resource_exists_(Req, State, classes);
resource_exists(Req, State={element_daggers, _}) ->
    resource_exists_(Req, State, daggers);
resource_exists(Req, State={element_straight_swords, _}) ->
    resource_exists_(Req, State, straight_swords).

resource_exists_(Req, State={Resource, ID}, Type) ->
    case dss_material:lookup(ID, Type) of
        {value, Material} ->
            {true, Req, {Resource, Material}};
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
content_types_provided(Req, State=collection_classes) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_list}], Req, State};
content_types_provided(Req, State=collection_daggers) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_list}], Req, State};
content_types_provided(Req, State=collection_straight_swords) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_list}], Req, State};

content_types_provided(Req, State={element_classes, _}) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_JSON}], Req, State};
content_types_provided(Req, State={element_daggers, _}) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_JSON}], Req, State};
content_types_provided(Req, State={element_straight_swords, _}) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_JSON}], Req, State}.


-spec element_to_list(cowboy_req:req(), state()) -> {[dss_material:material()], cowboy_req:req(), state()}.
element_to_list(Req, State={collection_classes, List}) ->
    element_to_list_(Req, State, classes);
element_to_list(Req, State={collection_daggers, List}) ->
    element_to_list_(Req, State, daggers);
element_to_list(Req, State={collection_straight_swords, List}) ->
    element_to_list_(Req, State, straight_swords).


-spec element_to_list_(
        cowboy_req:req()
      , state()
      , classes | daggers | straight_swords
    ) -> {[dss_material:material()], cowboy_req:req(), state()}.
element_to_list_(Req, State={_, List}, Type) ->
    Materials  = [element_to_json_value(Material, Type) || Material <- List],
    {ok, JSON} = jsone_encode:encode(Materials),
    {JSON, Req, State}.


-spec element_to_JSON(cowboy_req:req(), state()) -> {dss_material:class(), cowboy_req:req(), state()}.
element_to_JSON(Req, State={element_classes, Class}) ->
    element_to_JSON_(Req, State, classes);
element_to_JSON(Req, State={element_daggers, Dagger}) ->
    element_to_JSON_(Req, State, daggers);
element_to_JSON(Req, State={element_straight_swords, Dagger}) ->
    element_to_JSON_(Req, State, straight_swords).


-spec element_to_JSON_(
        cowboy_req:req()
      , state()
      , classes | daggers | straight_swords
    ) -> {dss_material:class(), cowboy_req:req(), state()}.
element_to_JSON_(Req, State={_, Material}, Type) ->
    {ok, JSON} = jsone_encode:encode(
        element_to_json_value(Material, Type)),
    {JSON, Req, State}.


-spec element_to_json_value(
        dss_material:material()
      , classes | daggers | straight_swords
    ) -> jsone:json_value().
element_to_json_value(Class, classes) ->
    {[
        {<<"id">>          , dss_material:id(Class)},
        {<<"name">>        , dss_material:name(Class)},
        {<<"levels">>      , dss_material:levels(Class)},
        {<<"vitality">>    , dss_material:vitality(Class)},
        {<<"attunement">>  , dss_material:attunement(Class)},
        {<<"endurance">>   , dss_material:endurance(Class)},
        {<<"strength">>    , dss_material:strength(Class)},
        {<<"dexterity">>   , dss_material:dexterity(Class)},
        {<<"resistance">>  , dss_material:resistance(Class)},
        {<<"intelligence">>, dss_material:intelligence(Class)},
        {<<"faith">>       , dss_material:faith(Class)}
    ]};
element_to_json_value(Material, Type) when Type == daggers; Type == straight_swords ->
    {[
        {<<"id">>          , dss_material:id(Material)},
        {<<"name">>        , dss_material:name(Material)},
        {<<"weight">>      , dss_material:weigth(Material)},
        {<<"requirements">>, dss_material:requirements(Material)}
    ]}.


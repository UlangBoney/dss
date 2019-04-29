-module(d_webui_classes_priv).
-behavior(cowboy_rest).
-export([
    init/2
  , allowed_methods/2
  , resource_exists/2
  , content_types_provided/2
  , element_to_JSON/2
]).

-include("dss_error.hrl").

-type resource() :: atom().
-type state()    :: resource()
                 | {resource(), dss_classes:class()}
                 | {resource(), dss_classes:id()}
                 .


-spec init(cowboy_req:req(), state()) -> {resource(), cowboy_req:req(), state()}.
init(Req, element) ->
    ClassID = cowboy_req:binding(classID, Req),
    {cowboy_rest, Req, {inited_element, ClassID}}.


-spec allowed_methods(cowboy_req:req(), state()) -> {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, {inited_element, ClassID}) ->
    {[<<"HEAD">>, <<"GET">>], Req, {allowed_element, ClassID}}.


-spec resource_exists(cowboy_req:req(), state()) -> {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State={provided_element, ClassID}) ->
    case dss_classes:lookup(ClassID) of
        {value, Class} ->
            {true, Req, {existed_element, Class}};
        none ->
            E    = ?DSS_CLASS_NOT_FOUND,
            Body = dss_error:error_to_JSON(E),
            Req2 = cowboy_req:set_resp_body(Body, Req),
            {false, Req2, State}
    end.


-spec content_types_provided(
        cowboy_req:req(), state()
    ) -> {[{binary() | {binary(), binary(), '*'
            | [{binary(), binary()}]}, atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, {allowed_element, ClassID}) ->
    {[{{<<"application">>, <<"json">>, '*'}, element_to_JSON}], Req, {provided_element, ClassID}}.


-spec element_to_JSON(cowboy_req:req(), state()) -> {dss_classes:class(), cowboy_req:req(), state()}.
element_to_JSON(Req, State={existed_element, Class}) ->
    {ok, JSON} = jsone_encode:encode(
        element_to_json_value(Class)),
    {JSON, Req, State}.


-spec element_to_json_value(dss_classes:class()) -> jsone:json_value().
element_to_json_value(Class) ->
    {[
        {<<"id">>          , dss_classes:id(Class)},
        {<<"name">>        , dss_classes:name(Class)},
        {<<"levels">>      , dss_classes:levels(Class)},
        {<<"vitality">>    , dss_classes:vitality(Class)},
        {<<"attunement">>  , dss_classes:attunement(Class)},
        {<<"endurance">>   , dss_classes:endurance(Class)},
        {<<"strength">>    , dss_classes:strength(Class)},
        {<<"dexterity">>   , dss_classes:dexterity(Class)},
        {<<"resistance">>  , dss_classes:resistance(Class)},
        {<<"intelligence">>, dss_classes:intelligence(Class)},
        {<<"faith">>       , dss_classes:faith(Class)}
    ]}.


-module(d_webui_sample_priv).
-behavior(cowboy_rest).
-export([
    init/2
  , allowed_methods/2
  , resource_exists/2
  , content_types_provided/2
  , element_to_JSON/2
]).

-type resource() :: atom().
-type state()    :: resource()
                 | {resource(), dss_sample:sample()}
                 .


-spec init(cowboy_req:req(), state()) -> {atom, cowboy_req:req(), state()}.
init(Req, element) ->
    {cowboy_rest, Req, inited_element}.


-spec allowed_methods(cowboy_req:req(), state()) -> {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, inited_element) ->
    {[<<"HEAD">>, <<"GET">>], Req, allowed_element}.


-spec resource_exists(cowboy_req:req(), state()) -> {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State=provided_element) ->
    case dss_sample:lookup() of
        {value, Sample} ->
            {true, Req, {existed_element, Sample}};
        none ->
            {false, Req, State}
    end.


-spec content_types_provided(
        cowboy_req:req(), state()
    ) -> {[{binary() | {binary(), binary(), '*'
            | [{binary(), binary()}]}, atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, allowed_element) ->
{[{{<<"application">>, <<"json">>, '*'}, element_to_JSON}], Req, provided_element}.


-spec element_to_JSON(cowboy_req:req(), state()) -> {dss_sample:sample(), cowboy_req:req(), state()}.
element_to_JSON(Req, State={existed_element, Sample}) ->
    {ok, JSON} = jsone_encode:encode(
        element_to_json_value(Sample)),
    {JSON, Req, State}.


-spec element_to_json_value(dss_sample:sample()) -> jsone:json_value().
element_to_json_value(Sample) ->
    {[
        {<<"name">>, dss_sample:name(Sample)}
    ]}.


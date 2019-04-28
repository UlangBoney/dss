-module(dss_sample).
-export_type([
    sample/0
]).
-export([
    lookup/0
  , name/1
]).

-opaque sample() ::
    #{ name => unicode:unicode_binary() }.


-spec lookup() -> dss_maybe:maybe(sample()).
lookup() ->
    DB   = <<"sample">>,
    Coll = <<"sampleColl">>,
    dss_mongodb:lookup(DB, Coll, {}, fun from_mongo_map/1).


-spec name(
        sample()
    ) -> unicode:unicode_binary().
name(Sample) ->
    maps:get(name, Sample).


-spec from_mongo_map(
        map()
    ) -> sample().
from_mongo_map(Map) ->
    #{ name => maps:get(<<"name">>, Map) }.


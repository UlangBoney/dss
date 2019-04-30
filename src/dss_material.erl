-module(dss_material).
-export_type([
    id/0
  , material/0
]).
-export([
    % * Read
    list/1
  , lookup/2
  , get/2

    % * Getters
  , id/1
  , name/1
  , levels/1
  , vitality/1
  , attunement/1
  , endurance/1
  , strength/1
  , dexterity/1
  , resistance/1
  , intelligence/1
  , faith/1
  , weigth/1
  , requirements/1
]).

-include("dss_error.hrl").

-type id() :: pos_integer().
-type material() :: class()
                 |  daggar()
                 | straight_sword()
                 .
-opaque class() ::
    #{ id           => id()
     , name         => #{ english  => unicode:unicode_binary()
                        , japanese => unicode:unicode_binary()}
     , levels       => pos_integer()
     , vitality     => pos_integer()
     , attunement   => pos_integer()
     , endurance    => pos_integer()
     , strength     => pos_integer()
     , dexterity    => pos_integer()
     , resistance   => pos_integer()
     , intelligence => pos_integer()
     , faith        => pos_integer()
    }.
-opaque daggar() ::
    #{ id           => id()
     , name         => #{ english  => unicode:unicode_binary()
                        , japanese => unicode:unicode_binary()}
     , weigth       => float()
     , requirements => requirements()
    }.
-opaque straight_sword() ::
    #{ id           => id()
     , name         => #{ english  => unicode:unicode_binary()
                        , japanese => unicode:unicode_binary()}
     , weigth       => float()
     , requirements => requirements()
    }.
-type requirements() ::
    #{ strength     => pos_integer()
     , dexterity    => pos_integer()
     , intelligence => pos_integer()
     , faith        => pos_integer()
    }.


-spec list(
        classes | daggers | straight_swords
    ) -> material().
list(Type) ->
    DB   = <<"dss_master">>,
    case Type of
        classes         -> dss_mongodb:cursor(DB, <<"classes">>, [], fun from_mongo_map/2, classes);
        daggers         -> dss_mongodb:cursor(DB, <<"equipment.weapons.daggers">>, [], fun from_mongo_map/2, daggers);
        straight_swords -> dss_mongodb:cursor(DB, <<"equipment.weapons.straightSwords">>, [], fun from_mongo_map/2, straight_swords)
    end.


-spec lookup(
        id()
      , classes | daggers | straight_swords
    ) -> dss_maybe:maybe(material()).
lookup(ID, Type) ->
    DB   = <<"dss_master">>,
    case Type of
        classes -> dss_mongodb:lookup(DB, <<"classes">>, {<<"_id">>, ID}, fun from_mongo_map/2, classes);
        daggers -> dss_mongodb:lookup(DB, <<"equipment.weapons.daggers">>, {<<"_id">>, ID}, fun from_mongo_map/2, daggers);
        straight_swords -> dss_mongodb:lookup(DB, <<"equipment.weapons.straightSwords">>, {<<"_id">>, ID}, fun from_mongo_map/2, straight_swords)
    end.


-spec get(
        id()
      , classes | daggers | straight_swords
    ) -> material().
get(ID, Type) ->
    case lookup(ID, Type) of
        {value, Material} -> Material;
        none -> dss_error:raise(?DSS_NOT_FOUND)
    end.


-spec id(material()) -> id().
id(Material) -> maps:get(id, Material).


-spec name(class()) -> unicode:unicode_binary().
name(Class) -> maps:get(name, Class).


-spec levels(class()) -> pos_integer().
levels(Class) -> maps:get(levels, Class).


-spec vitality(class()) -> pos_integer().
vitality(Class) -> maps:get(vitality, Class).


-spec attunement(class()) -> pos_integer().
attunement(Class) -> maps:get(attunement, Class).


-spec endurance(class()) -> pos_integer().
endurance(Class) -> maps:get(endurance, Class).


-spec strength(class()) -> pos_integer().
strength(Class) -> maps:get(strength, Class).


-spec dexterity(class()) -> pos_integer().
dexterity(Class) -> maps:get(dexterity, Class).


-spec resistance(class()) -> pos_integer().
resistance(Class) -> maps:get(resistance, Class).


-spec intelligence(class()) -> pos_integer().
intelligence(Class) -> maps:get(intelligence, Class).


-spec faith(class()) -> pos_integer().
faith(Class) -> maps:get(faith, Class).


-spec weigth(material()) -> float().
weigth(Material) -> maps:get(weight, Material).


-spec requirements(material()) -> requirements().
requirements(Material) -> maps:get(requirements, Material).


-spec from_mongo_map(
        map()
      , classes | daggers | straight_swords
    ) -> material().
from_mongo_map(MongoMap, classes) ->
    #{ id           => maps:get(<<"_id">>         , MongoMap)
     , name         => maps:get(<<"name">>        , MongoMap)
     , levels       => maps:get(<<"levels">>      , MongoMap)
     , vitality     => maps:get(<<"vitality">>    , MongoMap)
     , attunement   => maps:get(<<"attunement">>  , MongoMap)
     , endurance    => maps:get(<<"endurance">>   , MongoMap)
     , strength     => maps:get(<<"strength">>    , MongoMap)
     , dexterity    => maps:get(<<"dexterity">>   , MongoMap)
     , resistance   => maps:get(<<"resistance">>  , MongoMap)
     , intelligence => maps:get(<<"intelligence">>, MongoMap)
     , faith        => maps:get(<<"faith">>       , MongoMap)
    };
from_mongo_map(MongoMap, Type) when Type == daggers; Type == straight_swords ->
    #{ id           => maps:get(<<"_id">>         , MongoMap)
     , name         => maps:get(<<"name">>        , MongoMap)
     , weight       => maps:get(<<"weight">>      , MongoMap)
     , requirements => maps:get(<<"requirements">>, MongoMap)
    }.


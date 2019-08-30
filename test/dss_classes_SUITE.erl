-module(dss_classes_SUITE).
-export([
    all/0
]).
-export([
    classes_test/1
]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-spec all() -> [atom()].
all() ->
    [classes_test].


-spec classes_test([tuple()]) -> ok.
classes_test(_Config) ->
    ok = dss_mongodb:start_mongodb(),
    ?assertMatch(ok, lookup_test()),
    ?assertMatch(ok, list_test()),
    ok = application:stop(bson),
    ok = application:stop(poolboy),
    ok = application:stop(pbkdf2),
    ok = application:stop(mongodb).


-spec lookup_test() -> ok.
lookup_test() ->
    ok = lists:foreach(
        fun(ClassID) ->
            lookup_test(ClassID)
        end,
        lists:seq(0, 11)
    ).


-spec list_test() -> ok.
list_test() ->
    ClassesList = dss_classes:list(),
    10 = length(ClassesList),
    ok.


-spec lookup_test(pos_integer()) -> pos_integer().
lookup_test(0) ->
    none = dss_classes:lookup(0);
lookup_test(1) ->
    lookup_test(1, <<"warrior">>, <<"戦士">>, 4, 11, 8, 12, 13, 13, 11, 9, 9, 0);
lookup_test(2) ->
    lookup_test(2, <<"knight">>, <<"騎士">>, 5, 14, 10, 10, 11, 11, 10, 9, 11, 1);
lookup_test(3) ->
    lookup_test(3, <<"wanderer">>, <<"放浪者">>, 3, 10, 11, 10, 10, 14, 12, 11, 8, 1);
lookup_test(4) ->
    lookup_test(4, <<"thief">>, <<"盗人">>, 5, 9, 11, 9, 9, 15, 10, 12, 11, 1);
lookup_test(5) ->
    lookup_test(5, <<"bandit">>, <<"山賊">>, 4, 12, 8, 14, 14, 9, 11, 8, 10, 0);
lookup_test(6) ->
    lookup_test(6, <<"hunter">>, <<"狩人">>, 4, 11, 9, 11, 12, 14, 11, 9, 9, 0);
lookup_test(7) ->
    lookup_test(7, <<"sorcerer">>, <<"魔術師">>, 3, 8, 15, 8, 9, 11, 8, 15, 8, 3);
lookup_test(8) ->
    lookup_test(8, <<"pyromancer">>, <<"呪術師">>, 1, 10, 12, 11, 12, 9, 12, 10, 8, 2);
lookup_test(9) ->
    lookup_test(9, <<"cleric">>, <<"聖職者">>, 2, 11, 11, 9, 12, 8, 11, 8, 14, 1);
lookup_test(10) ->
    lookup_test(10, <<"deprived">>, <<"持たざるもの">>, 6, 11, 11, 11, 11, 11, 11, 11, 11, 1);
lookup_test(11) ->
    none = dss_classes:lookup(11).


-spec lookup_test(
        dss_classes:id(),
        unicode:unicode_binary(),
        unicode:unicode_binary(),
        pos_integer(),
        pos_integer(),
        pos_integer(),
        pos_integer(),
        pos_integer(),
        pos_integer(),
        pos_integer(),
        pos_integer(),
        pos_integer(),
        pos_integer()
    ) -> pos_integer().
lookup_test(ClassID, Ename, _Jname, Levels, Vitality, Attunement, Endurance
          , Strength, Dexterity, Resistance, Intelligence, Faith, AttSlots) ->
    {value, Class} = dss_classes:lookup(ClassID),
    ClassID        = maps:get(id, Class),
    Ename          = maps:get(english, maps:get(name, Class)),
    _              = maps:get(japanese, maps:get(name, Class)),
    Levels         = maps:get(levels, Class),
    Vitality       = maps:get(vitality, Class),
    Attunement     = maps:get(attunement, Class),
    Endurance      = maps:get(endurance, Class),
    Strength       = maps:get(strength, Class),
    Dexterity      = maps:get(dexterity, Class),
    Resistance     = maps:get(resistance, Class),
    Intelligence   = maps:get(intelligence, Class),
    Faith          = maps:get(faith, Class),
    AttSlots       = maps:get(attunementSlots, Class).


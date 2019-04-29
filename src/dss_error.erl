-module(dss_error).
-export([
    raise/1
  , raise/2
  , raise/3
  , fill/2
  , fill/3
  , error_to_JSON/1
]).

-include("dss_error.hrl").


-spec raise(#dss_error{}) -> no_return().
raise(E) ->
    erlang:error(E, [E]).


-spec raise(#dss_error{}, map()) -> no_return().
raise(E, PS) ->
    erlang:error(fill(E, PS), [E, PS]).


-spec raise(#dss_error{}, map(), jsone:json_value()) -> no_return().
raise(E, PS, C) ->
    erlang:error(fill(E, PS, C), [E, PS, C]).


-spec fill(#dss_error{}, map()) -> #dss_error{}.
fill(E, PS) ->
    E#dss_error{ params = PS }.


-spec fill(#dss_error{}, map(), jsone:json_value()) -> #dss_error{}.
fill(E, PS, C) ->
    E#dss_error{ params = PS, complement = {value,C}}.


-spec error_to_JSON(#dss_error{}) -> jsone:json_value().
error_to_JSON({dss_error, Reason, Message, _, _, _}) ->
    ReasonBin = atom_to_binary(Reason, utf8),
    <<"{\"", ReasonBin/binary, "\": ", "\"", Message/binary, "\"}">>.


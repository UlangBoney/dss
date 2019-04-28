-module(dss_maybe).
-export_type([
    maybe/1
]).

-type maybe(Any) :: {value, Any} | none .


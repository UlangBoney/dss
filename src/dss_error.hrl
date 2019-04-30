-ifndef(dss_error_hrl).
-record(dss_error, {
    reason  = 'DSS_ERROR' :: string(),
    message =  <<>>       :: unicode:unicode_binary(),
    status_code = none    :: dss_maybe:maybe(pos_integer()),
    params = #{}          :: map(),
    complement = none     :: dss_maybe:maybe(jsone:json_value())
}).


-define(DSS_INTERNAL_ERROR,
    #dss_error{
        reason  = 'DSS_INTERNAL_ERROR',
        status_code = 500,
        message = <<"An internal error occurred."/utf8>>
    }).

-define(DSS_INVALID_JSON,
    #dss_error{
        reason  = 'DSS_INVALID_JSON',
        status_code = 400,
        message = <<"JSON format is invalid."/utf8>>
    }).

-define(DSS_INVALID_JSON_PROPERTY,
    #dss_error{
        reason  = 'DSS_INVALID_JSON_PROPERTY',
        status_code = 400,
        message = <<"JSON property is invalid."/utf8>>
    }).

-define(DSS_NOT_FOUND,
    #dss_error{
        reason  = 'DSS_NOT_FOUND',
        status_code = 400,
        message = <<"Requested resource does not exist."/utf8>>
    }).

-endif.


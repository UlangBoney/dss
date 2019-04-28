-module(d_webui_sample).
-export([init/2]).

init(Req0, Opts) ->
    Req = cowboy_req:stream_reply(200, Req0),
    cowboy_req:stream_body("Welcome to Dark Souls Skill Simulator!", fin, Req),
    {ok, Req, Opts}.

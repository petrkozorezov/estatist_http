% @author Kozorezov Petr <petr.kozorezov@gmail.com>
% @copyright 2012 Kozorezov Petr
-module(estatist_http).

%% API
-export([
         start/0,
         stop/0
        ]).

start() ->
    application:load(?MODULE),
    ensure_deps_started(),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

ensure_deps_started() ->
    {ok, DepsList} = application:get_key(?MODULE, applications),
    [ensure_started(App) || App <-DepsList],
    ok.

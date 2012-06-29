% @author Kozorezov Petr <petr.kozorezov@gmail.com>
% @copyright 2012 Kozorezov Petr
-module(estatist_http_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Args), {Mod, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Config = application:get_all_env(),
    {ok, { {one_for_one, 5, 10}, [?CHILD(estatist_http_srv, [Config])]} }.


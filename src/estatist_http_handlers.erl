-module(estatist_http_handlers).
-behaviour(cowboy_http_handler).

%% Protocol:
%% -> Method: GET, URL: /?metric=all_metrics&type=all_types&param=all_params

%%
%% cowboy_http_handler behaviour
%%
-export([
         init/3,
         handle/2,
         terminate/2
        ]).

init({_Any, http}, Request, Options) ->
    [Peer, Method] = examine_request(Request, [peer, method]),
    %% ?LOG_DEBUG("New ~p request on ~p was received from ~s", [Method, RawPath, drimmi_ecore_utils:peername(Peer)]),
    {ok, Request, {Options, Peer, Method}}.

handle(Request, {_Options, _Peer, 'GET'}) ->
    {ok, Reply} =
        try
            MetricName      = get_qs_param(<<"metric">>, Request, <<"all_metrics">>),
            MetricType      = get_qs_param(<<"type">>,   Request, <<"all_types">>),
            MetricTypeParam = get_qs_param(<<"param">>,  Request, <<"all_params">>),
            MetricsValues   =
                case get_qs_param(<<"row_id">>,  Request, <<"undefined">>) of
                    undefined ->
                        estatist:get(MetricName, MetricType, MetricTypeParam);
                    RowID ->
                        estatist:get(MetricName, MetricType, MetricTypeParam, {id, RowID})
                end,
            %io:format("~p~n", [MetricsValues]),
            ProparedForJson = encode_response(MetricsValues),
            %io:format("~p~n", [ProparedForJson]),
            {ok, Json} = json:encode(ProparedForJson),
            cowboy_http_req:reply(200, [{'Content-Type', "application/json"}], Json, Request)
        catch
            error:badarg ->
                %%io:format("~p ~p~n", [badarg, erlang:get_stacktrace()]),
                reply(400, Request);
            _T:_E ->
                %%io:format("~p ~p~n", [E, erlang:get_stacktrace()]),
                reply(500, Request)
        end,
    {ok, Reply, undefined};

handle(Request, {_, _Peer, _Method}) ->
    %% ?LOG_ERROR("Bad request from '~s': ~p ~p", [drimmi_ecore_utils:peername(Peer), Method, Path]),
    {ok, Reply} = reply(400, Request),
    {ok, Reply, undefined}.


terminate(_Request, _State) ->
    ok.

reply(Code, Request) ->
    reply(Code, "", Request).

reply(Code, Data, Request) ->
    cowboy_http_req:reply(Code, [{'Content-Type', "text/html"}], [erlang:integer_to_list(Code) ++ " " ++ reply_data(Code), "<br/>", Data], Request).


reply_data(200) -> <<"Ok">>;
reply_data(400) -> <<"Bad request">>;
reply_data(404) -> <<"File not found">>;
reply_data(501) -> <<"Not implemented">>;
reply_data(500) -> <<"Internal server error">>;
reply_data(Code) -> erlang:list_to_binary("Error: " ++ erlang:integer_to_list(Code)).


examine_request(Request, What) ->
    [ begin {Value, _} = cowboy_http_req:Ask(Request), Value end || Ask <- What ].

get_qs_param(Param, Req, Def) ->
    {Val, _} = cowboy_http_req:qs_val(Param, Req, Def),
    list_to_existing_atom(binary_to_list(Val)).


encode_response(List) when is_list(List) ->
    {lists:map(fun(E) -> encode_response(E) end, List)};
encode_response({Name, Obj}) when is_list(Obj) or is_tuple(Obj) ->
    {Name, encode_response(Obj)};
encode_response(V) ->
    V.


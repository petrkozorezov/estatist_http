% @author Kozorezov Petr <petr.kozorezov@gmail.com>
% @copyright 2012 Kozorezov Petr
-module(estatist_http_handlers).
-behaviour(cowboy_http_handler).

%% Protocol:
%% -> Method: GET, URL: /?names=all&types=all&params=all
%% -> Method: GET, URL: /?names=a,b,c
%% -> Method: GET, URL: /?types=meter&params=one,five

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
    {ok, Request, {Options, Peer, Method}}.

handle(Request, {_Options, _Peer, 'GET'}) ->
    {ok, Reply} =
        try
            Query = [ 
                        {names,   get_qs_param(<<"names">>,    all, Request)},
                        {types,   get_qs_param(<<"types">>,    all, Request)},
                        {params,  get_qs_param(<<"params">>,   all, Request)}
                    ],
            SelectResult   =
                case get_qs_param(<<"row_id">>,  undefined, Request) of
                    undefined ->
                        estatist:select(Query);
                    RowID ->
                        estatist:select([{row_id, {id, RowID}} | Query])
                end,
            case SelectResult of
                {ok, MetricsValues} ->
                    ProparedForJson = encode_response(MetricsValues),
                    Json = jiffy:encode(ProparedForJson),
                    cowboy_http_req:reply(200, [{'Content-Type', "application/json"}], Json, Request);
                {error, Err} ->
                    Resp = 
                        case Err of
                            {unknown_metric, Name} ->
                                "unknown metric name: " ++ atom_to_list(Name);
                            {type_for_this_metric_not_found, Name, Type} ->
                                "type for this metric is not found: " ++ atom_to_list(Name) ++ ", " ++ atom_to_list(Type);
                            {incorrect_row_id, RowID1} ->
                                "incorrect row id: " ++ atom_to_list(RowID1)
                        end,
                    reply(400, Resp, Request)
            end
        catch
            throw:{bad_request,{unknown_atom, String}} ->
                %%io:format("~p ~p~n", [badarg, erlang:get_stacktrace()]),
                reply(400, "unknown atom: " ++ String, Request);
            _T:E ->
                error_logger:error_msg("~p ~p~n", [E, erlang:get_stacktrace()]),
                reply(500, Request)
        end,
    {ok, Reply, undefined};

handle(Request, {_, _Peer, _Method}) ->
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



encode_response(List) when is_list(List) ->
    {lists:map(fun(E) -> encode_response(E) end, List)};
encode_response({Name, Obj}) when is_list(Obj) or is_tuple(Obj) ->
    {encode_key(Name), encode_response(Obj)};
encode_response(V) ->
    V.

encode_key(Key) when is_list(Key) ->
    list_to_binary(Key);
encode_key(Key) ->
    Key.


get_qs_param(Key, Default, ReqData) ->
    case get_qs_atoms_list(Key, undefined, ReqData) of
        undefined -> Default;
        Values ->
            Values
    end.

get_qs_atoms_list(Field, Default, ReqData) ->
    case get_qs_value(Field, undefined, ReqData) of
        undefined -> Default;
        Values ->
            case [make_field_atom(Value) || Value <- tokens(Values)] of
                [V] ->
                    V;
                O ->
                    O
            end
    end.

tokens(Data) ->
    string:tokens(binary_to_list(Data), " \t,").

make_field_atom(Field) ->
    try list_to_existing_atom(Field)
    catch error:badarg -> throw({bad_request, {unknown_atom, Field}})
    end.

get_qs_value(Key, Default, ReqData) ->
    case cowboy_http_req:qs_val(Key, ReqData, undefined) of
        {undefined, _} ->
            Default;
        {Value, _} ->
            Value
    end.

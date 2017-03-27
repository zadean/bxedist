%%%----------------------------------------------------------------------
%%% File    : bxedist_app.erl
%%% Author  : Zachary Dean <contact@zadean.com>
%%% Purpose : 
%%% Created : 24 Mar 2017 by Zachary Dean <contact@zadean.com>
%%%----------------------------------------------------------------------

-module(bxedist_app).
-author('contact@zadean.com').
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
start(_Type, _StartArgs) ->
%%    Nodes = nodes(),
%%    ok = transfer_code(Nodes, bxedist_sup),
%%    ok = transfer_code(Nodes, bxd_node_sup),
%%    ok = transfer_code(Nodes, bxd_server),
%%    [erlang:spawn(Node, bxedist_sup, start, [])  || Node <- Nodes],
   %application:start(basexerl),
   bxedist_sup:start_link().

%% stop/1
stop(_State) ->
    ok.


%% transfer_code([], _Module) ->
%%     ok;
%% transfer_code([Node|Rest], Module) ->
%%     {_Module, Binary, Filename} = code:get_object_code(Module),
%%     rpc:call(Node, code, load_binary, [Module, Filename, Binary]),
%%     transfer_code(Rest, Module).
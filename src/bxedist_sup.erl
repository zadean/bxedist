%%%----------------------------------------------------------------------
%%% File    : bxedist_sup.erl
%%% Author  : Zachary Dean <contact@zadean.com>
%%% Purpose : Root Supervisor
%%% Created : 24 Mar 2017 by Zachary Dean <contact@zadean.com>
%%%----------------------------------------------------------------------
-module(bxedist_sup).
-author('contact@zadean.com').

-behaviour(supervisor).

-export([start_link/0, start/1, restart/1, stop/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
init([]) ->
   Servers = bxedist_config:server_ids(),
   ChildFn = fun(Name) ->
                {sfx(bxedist_db_sup, Name),
                  {bxedist_db_sup, start_link, [Name]},
                  permanent, 5000, supervisor, [bxedist_db_sup]}
             end,
   ChildSpec = lists:map(ChildFn, Servers),
   {ok,{{one_for_one,10,1}, ChildSpec}}.

sfx(Name, Suf) ->
   list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Suf)).

start(DBName) ->
   ChildSpec = {sfx(bxedist_db_sup, DBName),
                {bxedist_db_sup, start_link, [DBName]},
                permanent, 5000, supervisor, [bxedist_db_sup]},
   supervisor:start_child(?SERVER, ChildSpec).

restart(DBName) ->
   Name = sfx(bxedist_db_sup, DBName),
   supervisor:terminate_child(?SERVER, Name),
   supervisor:restart_child(?SERVER, Name).

stop(DBName) ->
   Name = sfx(bxedist_db_sup, DBName),
   supervisor:terminate_child(?SERVER, Name),
   supervisor:delete_child(?SERVER, Name).

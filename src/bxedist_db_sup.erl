%%%----------------------------------------------------------------------
%%% File    : bxedist_db_sup.erl
%%% Author  : Zachary Dean <contact@zadean.com>
%%% Purpose : DB Supervisor - one_for_all, no DB = no need for clients
%%% Created : 28 Mar 2017 by Zachary Dean <contact@zadean.com>
%%%----------------------------------------------------------------------
-module(bxedist_db_sup).
-author('contact@zadean.com').

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================

start_link(DBName) ->
   supervisor:start_link({local, sfx(?MODULE, DBName)}, ?MODULE, [DBName]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
init([DBName]) -> 
   ServConf   = bxedist_config:server_config(DBName),
   ClientConf = bxedist_config:client_config(DBName),
   ChildSpec = 
      case is_local(ServConf) of
         true ->
            [ % server
             {sfx(bxedist_server, DBName),
              {bxedist_server, start_link, [DBName, ServConf, self()]}, 
              permanent, 5000, worker, [bxedist_server]},
              % client sup
              {sfx(bxedist_client_sup, DBName), 
               {bxedist_client_sup, start_link, [DBName, ClientConf]}, 
               permanent, 5000, supervisor, [bxedist_client_sup]}
             ];
         _ -> %remote, so no server
            RemoteConf = get_remote_conf(ServConf),
            MergedConf = merge_configs(ServConf, RemoteConf),
            RemClientConf = bxedist_config:client_config_from_server(MergedConf),
            io:format("Remote Conf: ~p", [RemClientConf]),
            [{sfx(bxedist_client_sup, DBName), 
               {bxedist_client_sup, start_link, [DBName, RemClientConf]}, 
               permanent, 5000, supervisor, [bxedist_client_sup]}]
      end,
   {ok,{{one_for_all,10,1}, ChildSpec}}.

merge_configs(Cfg1, Cfg2) ->
   Dict1 = dict:from_list(Cfg1),
   Dict2 = dict:from_list(Cfg2),
   Fn = fun(_, V1, _) -> V1 end,
   Dict3 = dict:merge(Fn, Dict1, Dict2),
   dict:to_list(Dict3).

sfx(Name, Suf) ->
   list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Suf)).

is_local(Conf) ->
   {_, ConfHost} = bxedist_config:get_opt(host, Conf),
   {ok, Host} = inet:gethostname(),
   case ConfHost of
      "localhost" ->
         true;
      "127.0.0.1" ->
         true;
      _ ->
         ConfHost =:= Host
   end.

gv(Name, List) ->
   proplists:get_value(Name, List).

get_remote_conf(Options) ->
   Host = gv(host       , Options),
   Port = gv(serverport , Options),
   User = gv(user       , Options),
   Pass = gv(password   , Options),
   {ok, Pid} = basexerl:connect(Host, Port, User, Pass),
   {ok, Results} = basexerl:execute(Pid, "get"),
   RunOptList = parse_option_string(binary_to_list(Results)),
   RunOptList.

parse_option_string(OptionString) ->
   Tokens = string:tokens(OptionString, "\n"),
   IsOption = fun(Str) ->
                    case P = string:chr(Str, $:) of
                       0 ->
                          false;
                       _ ->
                          SStr = string:sub_string(Str, 1, P -1),
                          string:chr(SStr, $ ) =:= 0
                    end
              end,
   Fn = fun(Str) ->
              N = string:chr(Str, $:),
              P = string:sub_string(Str, 1, N -1),
              S = case length(Str) > N +1 of 
                     true -> 
                        string:sub_string(Str, N +1);
                     _ ->
                        ""
                  end,
              Name = list_to_atom(string:to_lower(P)),
              Value = string:strip(S),
              {Name, Value}
        end,
   List = [ Fn(Tok) || Tok <- Tokens, IsOption(Tok)],
   SetOpts = [Li || Li <- List, option_set(Li)],
   SetOpts.

option_set({_K,[]}) ->
   false;
option_set({_K, _V}) ->
   true.
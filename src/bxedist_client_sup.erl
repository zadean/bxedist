%% @author Zack
%% @doc @todo Add description to bxd_node_sup.


-module(bxedist_client_sup).
-behaviour(supervisor).
-export([init/1]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2]).
-export([add_client/2, remove_client/2, set_client_status/3, get_first_available/1]).

start_link(DBName, ClientOpts) ->
   SupName = sfx(?MODULE, DBName),
   supervisor:start_link({local, SupName}, ?MODULE, [DBName, ClientOpts]).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
%% ====================================================================
init([DBName, ClientOpts]) ->
   ets:new(DBName, [public, named_table]),
   %io:format("Started on ~s with pid ~p ~n", [node(), self()]),

   Parl = get_opt(parallel    , ClientOpts),
   Parr = case is_integer(Parl) of
             true ->
                Parl;
             _ ->
                list_to_integer(Parl)
         end,
   Host = get_opt(host        , ClientOpts),
   Port = get_opt(serverport  , ClientOpts),
   User = get_opt(user        , ClientOpts),
   Pass = get_opt(password    , ClientOpts),
   ClientArgs = [Host, Port, User, Pass],
   Clients = lists:map(fun(N) ->
                        UName = get_name(bxedist_client, DBName, N),
                        {UName, {bxedist_client, start_link, [ClientArgs, UName, DBName]}, permanent, 5000, worker, [bxedist_client]}
                       end, 
                       lists:seq(1, Parr)),
   {ok,{{one_for_one,10,1}, Clients}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


get_opt(Option, List) ->
   {_, Value} = lists:keyfind(Option, 1, List),
   Value.

get_name(Root, Serv, I) ->
   Base = atom_to_list(sfx(Root, Serv)),
   Suf = integer_to_list(I),
   Raw = Base ++ "_" ++ Suf,
   list_to_atom(Raw).

sfx(Name, Suf) ->
   list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Suf)).

add_client(DBName, ClientName) ->
   ets:insert(DBName, {ClientName, ready}).
remove_client(DBName, ClientName) ->
   ets:delete(DBName, ClientName).
set_client_status(DBName, ClientName, Status) ->
   ets:update_element(DBName, ClientName, {2, Status}).
get_first_available(DBName) ->
   case ets:match(DBName, {'$1', ready}) of 
      [[First]|_] ->
         First;
      _ ->
         case ets:first(DBName) of
            '$end_of_table' ->
               exit(normal);
            _ ->
               get_first_available(DBName)
         end
   end.
  



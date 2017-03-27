%% @author Zack
%% @doc @todo Add description to bxedist_commands.


-module(bxedist_commands).

-include("bxedist.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_cmd/1]). 

%%% gets the command text to send to the server via EXECUTE.
get_cmd(Command) ->
   list_to_binary(get_cmd1(Command)).

%% ====================================================================
%% Internal functions
%% ====================================================================


% ====================
% Database Operations
% ====================
get_cmd1(#create_db{name = Name, input = undefined}) ->
   [<<"create db ">>, Name];
get_cmd1(#create_db{name = Name, input = Input}) ->
   [<<"create db ">>, Name, <<" ">>, Input];
get_cmd1(#open{name = Name, path = undefined}) ->
   [<<"open ">>, Name];
get_cmd1(#open{name = Name, path = Path}) ->
   [<<"open ">>, Name, <<" ">>, Path];
get_cmd1(#check{name = Name}) ->
   [<<"check ">>, Name];
get_cmd1(#close{}) ->
   [<<"close">>];
get_cmd1(#export{path = Path}) ->
   [<<"export ">>, Path];
get_cmd1(#create_index{value = Value}) ->
   [<<"create index ">>, atom_to_binary(Value, latin1)];
get_cmd1(#drop_index{value = Value}) ->
   [<<"drop index ">>, atom_to_binary(Value, latin1)];
% ====================
% Administration
% ====================
get_cmd1(#alter_db{name = Name, newname = NewName}) ->
   [<<"alter db ">>, Name, <<" ">>, NewName];
get_cmd1(#drop_db{name = Name}) ->
   [<<"drop db ">>, Name];
get_cmd1(#create_backup{name = Name}) ->
   [<<"create backup ">>, Name];
get_cmd1(#restore{name = Name}) ->
   [<<"restore ">>, Name];
get_cmd1(#inspect{}) ->
   [<<"inspect">>];
get_cmd1(#drop_backup{name = Name}) ->
   [<<"drop backup ">>, Name];
get_cmd1(#show_backups{}) ->
   [<<"show backups">>];
get_cmd1(#copy{name = Name, newname = NewName}) ->
   [<<"copy ">>, Name, <<" ">>, NewName];
get_cmd1(#info_db{}) ->
   [<<"info db">>];
get_cmd1(#info_index{value = Value}) ->
   [<<"info index ">>, atom_to_binary(Value, latin1)];
get_cmd1(#info_storage{range_start = undefined, range_end = undefined}) ->
   [<<"info storage">>];
get_cmd1(#info_storage{range_start = RangeStart, range_end = RangeEnd}) ->
   [<<"info storage ">>, integer_to_binary(RangeStart), <<" ">>, integer_to_binary(RangeEnd)];
% ====================
% Querying
% ====================
get_cmd1(#list{name = undefined}) ->
   [<<"list">>];
get_cmd1(#list{name = Name, path = undefined}) ->
   [<<"list ">>, Name];
get_cmd1(#list{name = Name, path = Path}) ->
   [<<"list ">>, Name, <<" ">>, Path];
get_cmd1(#sxquery{query = Query}) ->
   [<<"xquery ">>, Query];
get_cmd1(#find{query = Query}) ->
   [<<"find ">>, Query];
get_cmd1(#test{path = Path}) ->
   [<<"test ">>, Path];
get_cmd1(#repo_install{path = Path}) ->
   [<<"repo install ">>, Path];
get_cmd1(#repo_list{}) ->
   [<<"repo list">>];
get_cmd1(#repo_delete{name = Name}) ->
   [<<"repo delete ">>, Name];
% ====================
% Updates
% ====================
get_cmd1(#add{path = undefined, input = Input}) ->
   [<<"add ">>, Input];
get_cmd1(#add{path = Path, input = Input}) ->
   [<<"add to ">>, Path, <<" ">>, Input];
get_cmd1(#delete{path = Path}) ->
   [<<"delete ">>, Path];
get_cmd1(#rename{path = Path, newpath = NewPath}) ->
   [<<"rename ">>, Path, <<" ">>, NewPath];
get_cmd1(#replace{path = Path, input = Input}) ->
   [<<"replace ">>, Path, <<" ">>, Input];
get_cmd1(#optimize{all = true}) ->
   [<<"optimize all">>];
get_cmd1(#optimize{}) ->
   [<<"optimize">>];
get_cmd1(#flush{}) ->
   [<<"flush">>];
% ====================
% Monitoring
% ====================
get_cmd1(#show_sessions{}) ->
   [<<"show sessions">>];
get_cmd1(#show_users{database = undefined}) ->
   [<<"show users">>];
get_cmd1(#show_users{database = Database}) ->
   [<<"show users on ">>, Database];
get_cmd1(#kill{target = Target}) ->
   [<<"kill ">>, Target];
get_cmd1(#jobs_list{}) ->
   [<<"jobs list">>];
get_cmd1(#jobs_result{id = Id}) ->
   [<<"jobs result ">>, Id];
get_cmd1(#jobs_stop{id = Id}) ->
   [<<"jobs stop ">>, Id];
% ====================
% User Management
% ====================
get_cmd1(#create_user{name = Name, password = undefined}) ->
   [<<"create user ">>, Name];
get_cmd1(#create_user{name = Name, password = Password}) ->
   [<<"create user ">>, Name, <<" ">>, Password];
get_cmd1(#alter_user{name = Name, newname = NewName}) ->
   [<<"alter user ">>, Name, <<" ">>, NewName];
get_cmd1(#alter_password{name = Name, password = undefined}) ->
   [<<"alter password ">>, Name];
get_cmd1(#alter_password{name = Name, password = Password}) ->
   [<<"alter password ">>, Name, <<" ">>, Password];
get_cmd1(#drop_user{name = Name, pattern = undefined}) ->
   [<<"drop user ">>, Name];
get_cmd1(#drop_user{name = Name, pattern = Pattern}) ->
   [<<"drop user ">>, Name, <<" on ">>, Pattern];
get_cmd1(#grant{right = Right, user = Name, pattern = undefined}) ->
   [<<"grant ">>, atom_to_binary(Right, latin1), <<" to ">>, Name];
get_cmd1(#grant{right = Right, user = Name, pattern = Pattern}) ->
   [<<"grant ">>, atom_to_binary(Right, latin1), <<" on ">>, Pattern, <<" to ">>, Name];
get_cmd1(#password{password = Password}) ->
   [<<"password ">>, Password];
% ====================
% General Commands
% ====================
get_cmd1(#run{file = File}) ->
   [<<"run ">>, File];
get_cmd1(#execute{input = Input}) ->
   [<<"execute ">>, Input];
get_cmd1(#get_option{option = undefined}) ->
   [<<"get">>];
get_cmd1(#get_option{option = Option}) ->
   [<<"get ">>, atom_to_binary(Option, latin1)];
get_cmd1(#set_option{option = Option, value = undefined}) ->
   [<<"set ">>, atom_to_binary(Option, latin1)];
get_cmd1(#set_option{option = Option, value = Value}) when is_integer(Value) ->
   [<<"set ">>, atom_to_binary(Option, latin1), <<" ">>, integer_to_binary(Value)];
get_cmd1(#set_option{option = Option, value = Value}) when is_atom(Value) ->
   [<<"set ">>, atom_to_binary(Option, latin1), <<" ">>, atom_to_binary(Value, latin1)];
get_cmd1(#set_option{option = Option, value = Value}) ->
   [<<"set ">>, atom_to_binary(Option, latin1), <<" ">>, Value];
get_cmd1(#info{}) ->
   [<<"info">>];
get_cmd1(#help{command = undefined}) ->
   [<<"help">>];
get_cmd1(#help{command = Command}) ->
   [<<"help ">>, Command];

get_cmd1(#transaction{commands = Commands}) ->
   Fn = fun(A) -> get_cmd(A) end,
   List = lists:map(Fn, Commands),
   [<<"execute ">>, lists:foldl(fun (E, Acc) -> [Acc, <<"; ">>, E] end, hd(List), tl(List))];

get_cmd1(_Other) ->
   {error, badarg}.

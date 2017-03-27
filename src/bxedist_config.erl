%% @author Zack
%% @doc @todo Add description to bxd_config.


-module(bxedist_config).

-define(APP, bxedist).
-define(PRIV, code:priv_dir(?APP)).
-define(CONFIG, code:lib_dir(?APP, config)).
-define(APPDAT, filename:basedir(user_data, atom_to_list(?APP))).

%% ====================================================================
%% API functions
%% ====================================================================
-export([server_config/1,
         client_config/1,
         client_config_from_server/1,
         server_ids/0,
         get_jar_loc/0,
         get_opt/2
         ]). 

%% ====================================================================
%% Internal functions
%% ====================================================================

server_ids() ->
   Cfg = read_config(),
   [Id || {Id, _} <- Cfg  ].

server_config(Name) ->
   Cfg = read_config(),
   Def = dict:from_list(get_def_options(Name)),
   Ovr = dict:from_list(lists:flatten([Opts || {Id, Opts} <- Cfg, Id =:= Name])),
   MrgFn = fun(_Key, Value1, _Value2) -> Value1 end,
   Mrg = dict:merge(MrgFn, Ovr, Def),
   dict:to_list(Mrg).

client_config(Name) ->
   Svr = server_config(Name),
   client_config_from_server(Svr).

client_config_from_server(ServConf) ->
   [
      get_opt(host      , ServConf),
      to_int(get_opt(serverport, ServConf)),
      get_opt(user      , ServConf),
      get_opt(password  , ServConf),
      to_int(get_opt(parallel  , ServConf))
   ].

to_int({K, Str}) when is_list(Str) ->
   {K, list_to_integer(Str)};
to_int({K, Str}) when is_integer(Str) ->
   {K, Str}.

get_jar_loc() ->
   Jars = filelib:wildcard("./*.jar", ?PRIV),
   Sorted = lists:sort(Jars),
   filename:join(?PRIV, lists:last(Sorted)).

get_opt(Option, List) ->
   lists:keyfind(Option, 1, List).

read_config() ->
   Cfg = filename:join(?CONFIG, "bxedist.config"),
   {ok, [Terms]} = file:consult(Cfg),
   Terms.

node_path() ->
   Node = atom_to_list(node()),
   [Name, Host] = string:tokens(Node, "@"),
   Host ++ "/" ++ Name.

sub_dir(Id) ->
   node_path()++"/"++atom_to_list(Id).

get_def_options(Id) ->
[
{path          , user_data_sub(sub_dir(Id))},
{debug         , false}, 
{dbpath        , user_data_sub(sub_dir(Id)++"/data") }, 
{logpath       , user_data_sub(sub_dir(Id)++"/logs") }, 
{repopath      , user_data_sub(sub_dir(Id)++"/repo") }, 
{lang          , "English"}, 
{langkey       , false}, 
{fairlock      , false}, 
{cachetimeout  , 3600}, 
{host          , get_host_name()}, 
{port          , 60000}, 
{serverport    , 60000}, 
{user          , "admin"}, 
{password      , "admin"}, 
{authmethod    , "Basic"}, 
{serverhost    , []}, 
{proxyhost     , []}, 
{proxyport     , 0}, 
{nonproxyhosts , []}, 
{ignorecert    , false}, 
{timeout       , 30}, 
{keepalive     , 0}, % no timeout for inactivity 
{parallel      , 8}, 
{log           , true}, 
{logmsgmaxlen  , 1000}, 
{webpath       , user_data_sub(sub_dir(Id)++"/webapp")}, 
{restxqpath    , []}, 
{parserestxq   , 3}, 
{restpath      , []}, 
{httplocal     , false}, 
{stopport      , 8985}, 
{mainmem       , false}, 
{addcache      , false}, 
{createfilter  , "*.xml"}, 
{addarchives   , true}, 
{archivename   , false}, 
{skipcorrupt   , false}, 
{addraw        , false}, 
{parser        , "xml"}, 
{csvparser     , []}, 
{jsonparser    , []}, 
{htmlparser    , []}, 
{textparser    , []}, 
{chop          , true}, 
{stripns       , false}, 
{intparse      , false}, 
{dtd           , false}, 
{xinclude      , true}, 
{catfile       , []}, 
{textindex     , true}, 
{attrindex     , true}, 
{tokenindex    , true}, 
{ftindex       , false}, 
{textinclude   , []}, 
{attrinclude   , []}, 
{tokeninclude  , []}, 
{ftinclude     , []}, 
{maxlen        , 96}, 
{maxcats       , 100}, 
{updindex      , false}, 
{autooptimize  , false}, 
{splitsize     , 0}, 
{stemming      , false}, 
{casesens      , false}, 
{diacritics    , false}, 
{language      , "en"}, 
{stopwords     , []}, 
{queryinfo     , false}, 
{xquery3       , true}, 
{mixupdates    , false}, 
{bindings      , []}, 
{inlinelimit   , 100}, 
{tailcalls     , 256}, 
{defaultdb     , false}, 
{forcecreate   , false}, 
{checkstrings  , true}, 
{lserror       , 0}, 
{runquery      , true}, 
{runs          , 1}, 
{serialize     , true}, 
{serializer    , "indent=no"}, % no pretty print 
{exporter      , []}, 
{xmlplan       , false}, 
{compplan      , true}, 
{dotplan       , false}, 
{dotcompact    , false}, 
{autoflush     , true}, 
{writeback     , false}, 
{maxstat       , 30}   
].

get_host_name() ->
  {ok, Host} = inet:gethostname(),
  Host.

user_data_sub(Sub) ->
   filename:join(?APPDAT, Sub).


%% @author Zack
%% @doc @todo Add description to bxd_server.


-module(bxedist_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, stop/1]).
-export([start_link/3]).

-record(state, {
               db_options :: [{atom(), term()}], 
               jvm        :: string(), 
               jar_loc    :: string(), 
               ext_port   :: port() | undefined,
               servername :: atom(),
               parent     :: pid()}).

start_link(Name, Options, Parent) ->
   gen_server:start_link({local, Name}, ?MODULE, [Name, Options, Parent], []).

start_server(State) ->
   Path = gv(path, State#state.db_options),
   Cmd = cmd_string(State),
   Port = cmd_port(Cmd, Path),
   NewState = State#state{ext_port = Port},
   NewState.

stop(Name) ->
   gen_server:call(Name, stop_server).

start(Name) ->
   gen_server:call(Name, start_server).

stop_server(State) ->
   cmd_stop(State),
   NewState = State#state{ext_port = undefined},
   NewState.


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Name, Options, Parent]) ->
   process_flag(trap_exit, true),
   JVM = case os:find_executable("java") of
            [] ->
               throw({stop, no_jvm_on_path});
            _ ->
               "java"
         end,
   Jar  = bxedist_config:get_jar_loc(), 
   NewState = #state{db_options = Options, jar_loc = Jar, jvm = JVM, servername = Name, parent = Parent},
   Host = gv(host       , Options),
   Port = gv(serverport , Options),
   case check_port_in_use(Host, Port) of
      true ->
         _ = stop_server(NewState),
         receive
            _ ->
               ok
         after 5000 ->
             ok
         end,
         StartedState = start_server(NewState),
         {ok, StartedState, hibernate};
      _ ->
         StartedState = start_server(NewState),
         {ok, StartedState, hibernate}
   end. 


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(start_server, _From, #state{servername = Name, ext_port = undefined} = State) ->
   NewState = start_server(State),
   Reply = {Name, started},
   {reply, Reply, NewState}; 
handle_call(stop_server, _From, #state{servername = Name} = State) ->
   StoppedState = stop_server(State),
   Reply = {Name, stopped},
   {reply, Reply, StoppedState}; 
handle_call(_Request, _From, State) ->
   Reply = ok,
   {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({Port, {exit_status, 0}}, #state{servername = Servername, ext_port = Port} = State) -> 
   % server stopped by API so don't restart
   io:format("~s\tServer stopped.~n", [atom_to_list(Servername)]),
   NewState = State#state{ext_port = undefined},
   {noreply, NewState};
handle_info({Port, {exit_status, Num}}, #state{servername = Servername, ext_port = Port} = State) -> 
   % server stopped by crash so restart
   io:format("~s\tServer stopped with signal: ~B~n", [atom_to_list(Servername), Num]),
   NewState = State#state{ext_port = undefined},
   {stop, normal, NewState};
handle_info({_Port, {exit_status, _Num}}, State) -> 
   % ignore one-off exits 
   {noreply, State};
handle_info({'EXIT', Port, _}, State) when Port =/= State#state.ext_port ->
   % ignore one-off exits 
   {noreply, State};
handle_info({_Port, {data, {eol, Line}}}, #state{servername = Servername} = State) -> 
   % logging the console
   io:format("~s\t~s~n", [atom_to_list(Servername), Line]),
   {noreply, State};
handle_info(Info, State) ->
   io:format("Unexpected info: ~p~n", [Info]),
   {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, #state{ext_port = undefined}) ->
   io:format("Server down~n"),
   ok;
terminate(_Reason, State) ->
   io:format("Got terminate shutting down server~n"),
   _ = stop_server(State),
   ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 


%% ====================================================================
%% Internal functions
%% ====================================================================

check_port_in_use(Host, Port) ->
   case gen_tcp:connect(Host, Port, [], 1000) of
      {ok, S} ->
         ok = gen_tcp:close(S),
         io:format("Port ~p in use on ~p~n", [Port, Host]),
         true;
      _ ->
         false
   end.

gv(Name, List) ->
   proplists:get_value(Name, List).

cmd_string(#state{jvm = JVM, jar_loc = Jar, db_options = RawOpts}) ->
   StrVal = fun(Str) when is_atom(Str) ->
                  atom_to_list(Str);
               (Str) when is_integer(Str) ->
                  integer_to_list(Str);
               (Str) when is_binary(Str) ->
                  binary_to_list(Str);
               (Str) ->
                  Str
            end,
   Flags = [lists:flatten([" -Dorg.basex.",io_lib:write(Oname),"=",StrVal(Ovalue)]) || {Oname, Ovalue} <- RawOpts, Ovalue =/= []],
   lists:flatten([
                  JVM,
                  " -classpath ", Jar,
                  " -Xmx1024m",
                  Flags,
                  " org.basex.BaseXServer"
                  ]).

cmd_stop(#state{jvm = JVM, jar_loc = Jar, db_options = RawOpts}) ->
   StrVal = fun(Str) when is_atom(Str) ->
                  atom_to_list(Str);
               (Str) when is_integer(Str) ->
                  integer_to_list(Str);
               (Str) when is_binary(Str) ->
                  binary_to_list(Str);
               (Str) ->
                  Str
            end,
   Take = [{host, gv(host, RawOpts)}, {serverport, gv(serverport, RawOpts)}],
   Flags = [lists:flatten([" -Dorg.basex.",io_lib:write(Oname),"=",StrVal(Ovalue)]) || {Oname, Ovalue} <- Take, Ovalue =/= []],
   Cmd = lists:flatten([
                  JVM,
                  " -classpath ", Jar,
                  Flags,
                  " org.basex.BaseXServer stop"
                  ]),
   os:cmd(Cmd).

cmd_port(Cmd, Path) ->
   PortSettings = [{line, 1000}, 
                   {cd, Path},
                   stderr_to_stdout,
                   hide,          % windows
                   binary,
                   exit_status,
                   overlapped_io  % windows
                  ],
   catch erlang:open_port({spawn, Cmd}, PortSettings).


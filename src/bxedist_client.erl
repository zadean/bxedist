%% @author Zack
%% @doc @todo Add description to bxd_client.


-module(bxedist_client).
-behaviour(gen_server).

-include("bxedist.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/3, query/2]).

start_link(Args, UName, DBName) ->
    gen_server:start_link({local, UName},?MODULE, [Args, UName, DBName], []).

query(DBName, Query) ->
   Client = bxedist_client_sup:get_first_available(DBName),
   busy(DBName, Client),
   Result = gen_server:call(Client, Query, 60000),
   free(DBName, Client),
   Result.


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {
               connection, 
               dbname,
               uname}).

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
init([[Host, Port, User, Pass], UName, DBName]) -> 
   case basexerl:connect(Host, Port, User, Pass) of
      {ok, Pid} ->
         bxedist_client_sup:add_client(DBName, UName),
         {ok, #state{connection = Pid, dbname = DBName, uname = UName}};
      Err ->
         {stop, Err}
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
% XQuery handlers
handle_call(#xquery{statement = Statement, 
                    context = undefined, 
                    variables = undefined, 
                    result_type = RType}, _From, 
            #state{connection = Conn} = State) 
   ->
   {ok, Qid} = basexerl:query(Conn, Statement),
   Reply = exec_qry_by_ret_type(RType, Conn, Qid),
   _ = basexerl:q_close(Conn, Qid),
   {reply, Reply, State};

handle_call(#xquery{statement = Statement, 
                    context = Context, 
                    variables = undefined, 
                    result_type = RType}, _From, 
            #state{connection = Conn} = State) 
   ->
   {ok, Qid} = basexerl:query(Conn, Statement),
   ok = bind_context(Conn, Qid, Context), 
   Reply = exec_qry_by_ret_type(RType, Conn, Qid),
   _ = basexerl:q_close(Conn, Qid),
   {reply, Reply, State};
handle_call(#xquery{statement = Statement, 
                    context = undefined, 
                    variables = Variables, 
                    result_type = RType}, _From, 
            #state{connection = Conn} = State) 
   ->
   {ok, Qid} = basexerl:query(Conn, Statement),
   ok = bind_variables(Conn, Qid, Variables),
   Reply = exec_qry_by_ret_type(RType, Conn, Qid),
   _ = basexerl:q_close(Conn, Qid),
   {reply, Reply, State};
handle_call(#xquery{statement = Statement, 
                    context = Context, 
                    variables = Variables, 
                    result_type = RType}, _From, 
            #state{connection = Conn} = State) 
   ->
   {ok, Qid} = basexerl:query(Conn, Statement),
   ok = bind_context(Conn, Qid, Context), 
   ok = bind_variables(Conn, Qid, Variables),
   Reply = exec_qry_by_ret_type(RType, Conn, Qid),
   _ = basexerl:q_close(Conn, Qid),
   {reply, Reply, State};
% commands that can happen directly with the client when not in transaction
handle_call(#add{path = Path, input = Input}, _From, #state{connection = Conn} = State) ->
   Reply = basexerl:add(Conn, Path, Input),
   {reply, Reply, State};
handle_call(#replace{path = Path, input = Input}, _From, #state{connection = Conn} = State) ->
   Reply = basexerl:replace(Conn, Path, Input),
   {reply, Reply, State};
handle_call(#create_db{name = Name, input = undefined}, _From, #state{connection = Conn} = State) ->
   Reply = basexerl:create(Conn, Name),
   {reply, Reply, State};
handle_call(#create_db{name = Name, input = Input}, _From, #state{connection = Conn} = State) ->
   Reply = basexerl:create(Conn, Name, Input),
   {reply, Reply, State};
% due to binary encoding/decoding, seperate from the other commands
% cannot be in a transaction
handle_call(#store{path = Path, input = Input}, _From, #state{connection = Conn} = State) ->
   Reply = basexerl:store(Conn, Path, Input),
   {reply, Reply, State};
handle_call(#retrieve{path = Path}, _From, #state{connection = Conn} = State) ->
   Reply = basexerl:retrieve(Conn, Path),
   {reply, Reply, State};
% normally encoded commands
handle_call(Command, _From, #state{connection = Conn} = State) ->
   Exec = bxedist_commands:get_cmd(Command),
   Reply = basexerl:execute(Conn, Exec),
   {reply, Reply, State};

%% fall-through
handle_call(Request, _From, State) ->
   io:format("Unexpected call: ~p~n", [Request]),
   Reply = ok,
   {reply, Reply, State}.

busy(Db, U) ->
   bxedist_client_sup:set_client_status(Db, U, busy).
free(Db, U) ->
   bxedist_client_sup:set_client_status(Db, U, ready).


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
handle_info(Info, State) ->
   io:format("got info on client: ~p~n", [Info]),
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
terminate(Reason, #state{connection = Conn, dbname = DBName, uname = UName}) ->
   io:format("got terminate on client with reason: ~p~n",[Reason]),
   bxedist_client_sup:remove_client(DBName, UName),
   basexerl:disconnect(Conn),
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


get_context_seq(CSeq) ->
     Fn = fun(#context{value = CValue, xstype = CType}) ->
              case CType of
                 undefined ->
                    {CValue};
                 _ ->
                    {CValue, CType}
              end
        end,
   CtxList = [Fn(V) || V <- CSeq],
   {context, CtxList}.

exec_qry_by_ret_type(RType, Conn, Qid) ->
   case RType of
      single ->
         basexerl:q_execute(Conn, Qid);
      sequence ->
         basexerl:q_results(Conn, Qid)
   end.

bind_variables(_Conn, _Qid, undefined) ->
   ok;
bind_variables(Conn, Qid, VarList) ->
   Fn = fun(V) ->
              bind_variable(V, Conn, Qid)
        end,
   lists:foreach(Fn, VarList),
   ok.

bind_variable(#variable{name = Name, value = Value, xstype = Xstype}, Conn, Qid) ->
   case Xstype of
      undefined ->
         basexerl:q_bind(Conn, Qid, Name, Value);
      _ ->
         basexerl:q_bind(Conn, Qid, Name, Value, Xstype)
   end;
bind_variable(#variable_seq{name = Name, values = Values}, Conn, Qid) ->
     Fn = fun(#item{value = Value, xstype = Xstype}) ->
              case Xstype of
                 undefined ->
                    {Value};
                 _ ->
                    {Value, Xstype}
              end
        end,
   VarList = [Fn(V) || V <- Values],
   basexerl:q_bind(Conn, Qid, {Name, VarList}).

bind_context(_Conn, _Qid, undefined) ->
   ok;
bind_context(Conn, Qid, #context{value = CValue, xstype = CType}) -> 
   case CType of
      undefined ->
         basexerl:q_context(Conn, Qid, CValue);
      _ ->
         basexerl:q_context(Conn, Qid, CValue, CType)
   end,
   ok; 
bind_context(Conn, Qid, #context_seq{values = CValues}) ->
   CtxList = get_context_seq(CValues),
   basexerl:q_context(Conn, Qid, CtxList),
   ok.

%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(orchestrate). 

-behaviour(gen_server). 
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
-define(SERVER,?MODULE).
-define(Interval,20*1000).

%% External exports
-export([
	 loop/0,
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	
	       }).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% Gen server functions

start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).

%% ====================================================================
%% Application handling
%% ====================================================================

%%---------------------------------------------------------------
%% Function: loop()
%% @doc: loop        
%% @param: non
%% @returns:ok
%%
%%---------------------------------------------------------------
-spec loop()-> ok.
loop()->
    gen_server:cast(?SERVER,{loop}).


%% ====================================================================
%% Support functions
%% ====================================================================

%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> pong.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).

%% ====================================================================
%% Gen Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    spawn(fun()->local_loop() end),

    {ok, #state{}
    }.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------



handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call({stopped},_From, State) ->
    Reply=ok,
    {reply, Reply, State};


handle_call({not_implemented},_From, State) ->
    Reply=not_implemented,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched call",[Request, From])]),
    Reply = {ticket,"unmatched call",Request, From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_cast({loop}, State) ->
    spawn(fun()->local_loop() end),
    {noreply, State};

handle_cast(_Msg, State) ->
  %  rpc:cast(node(),log,log,[?Log_ticket("unmatched cast",[Msg])]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched info",[Info])]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%{{AppId,ApplVsn},GitPath}

local_loop()->
%    io:format("nodes() ~p~n",[nodes()]),
%    io:format("sd:all() ~p~n",[sd:all()]),

    timer:sleep(?Interval),
    {ok,WantedHost}=inet:gethostname(),
    case rpc:call(node(),lib_appl,service_specs_info,[],10*1000) of
	{badrpc,_}->
	    ok;
	ServiceSpecsInfo->
%	    io:format("ServiceSpecsInfo ~p~n",[{?MODULE,?LINE,ServiceSpecsInfo}]),    
	    AppsToStart=[{ApplId,ApplVsn}||{{ApplId,ApplVsn},_GitPath}<-ServiceSpecsInfo,
					   []=:=sd:get_host(list_to_atom(ApplId),WantedHost)],
%	    io:format("AppsToStart ~p~n",[{?MODULE,?LINE,AppsToStart}]),    
%	    StartR=[{{ApplId,ApplVsn},load_start(ApplId,ApplVsn)}||{ApplId,ApplVsn}<-AppsToStart],
	    
	   % io:format("StartR ~p~n",[{?MODULE,?LINE,StartR}])
	    load_start(AppsToStart,[])
    end,
   
    

    rpc:cast(node(),?MODULE,loop,[]).
		  

load_start([],Result)->
    Result;

load_start([{ApplId,ApplVsn}|T],Acc)->
    StartR=case controller:create_vm() of
	      {ok,Vm}->
		  case controller:load_start_appl(ApplId,ApplVsn,Vm) of
		      ok->
			  nodelog_server:log(notice,?MODULE_STRING,?LINE,"application started "++ApplId),
			  ok;
		      Err->
			  {error,Err}
		  end;
	      Err->
		  {error,Err}
	  end,

    timer:sleep(2000),
    load_start(T,[{ApplId,ApplVsn,StartR}|Acc]).

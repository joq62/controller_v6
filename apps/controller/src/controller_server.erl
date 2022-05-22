%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(controller_server).  

-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").

%% --------------------------------------------------------------------
-define(SERVER,?MODULE).
-define(ControllerEbin,"controller_app/ebin").
-define(LogDir,"logs").
%% External exports
-export([
	 all_specs/0,
	 create_vm/0,
	 load_start_appl/3,
	 stop_unload_appl/3,
	 delete_vm/1,
	 
	 get_spec/2,
	 actual_state/0,
	 load_read_specs/0,
	
	 read_state/0,
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		vm_list,
		service_specs_info
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
%  create_vm/0,
%  load_start_appl/3,
%  stop_unload_appl/2,
%  delete_vm/1,

%%---------------------------------------------------------------
%% Function: load_start_appl(ApplId,ApplVsn,Node)
%% @doc: loads and starts App ApplId,ApplVsn on slave Node         
%% @param: ApplId,ApplVsn,Node
%% @returns:ok|{error,Reason}
%%
%%---------------------------------------------------------------
-spec load_start_appl(string(),string(),node())-> ok|{error,term()}.
load_start_appl(ApplId,ApplVsn,Node)->
    gen_server:call(?SERVER, {load_start_appl,ApplId,ApplVsn,Node},infinity).

%%---------------------------------------------------------------
%% Function: stop_unload_appl(ApplId,ApplVsn,Node)
%% @doc: stopps and unloads  App ApplId,ApplVsn on slave Node         
%% @param: ApplId,ApplVsn,Node
%% @returns:ok|{error,Reason}
%%
%%---------------------------------------------------------------
-spec stop_unload_appl(string(),string(),node())-> ok|{error,term()}.
stop_unload_appl(ApplId,ApplVsn,Node)->
    gen_server:call(?SERVER, {stop_unload_appl,ApplId,ApplVsn,Node},infinity).

%%---------------------------------------------------------------
%% Function: create_vm()
%% @doc: creates a slave and dir to load appl         
%% @param: non
%% @returns:{ok,Node}|{error,Reason}
%%
%%---------------------------------------------------------------
-spec create_vm()->  {ok,node()}|{error,term()}.
create_vm()->
    gen_server:call(?SERVER, {create_vm},infinity).
%%---------------------------------------------------------------
%% Function: delete_vm(Node)
%% @doc: stops the slave and deletes related dir         
%% @param: non
%% @returns:ok|{error,Reason}
%%
%%---------------------------------------------------------------
-spec delete_vm(node())-> ok|{error,term()}.
delete_vm(Node)->
    gen_server:call(?SERVER, {delete_vm,Node},infinity).

%%---------------------------------------------------------------
%% Function: get_spec(Name,Vsn)
%% @doc: retreive Service info        
%% @param: Name,Vsn
%% @returns:[SeviceSpecInfo]|{error,eexists}
%%
%%---------------------------------------------------------------
-spec get_spec(string(),string())->  [term()]|{atom(),list()}.
get_spec(Name,Vsn)->
    gen_server:call(?SERVER, {get_spec,Name,Vsn},infinity).

%%---------------------------------------------------------------
%% Function: actual_state()
%% @doc: evaluta Id and Vms to start and delete        
%% @param: non
%% @returns:[{Id,ToStart,ToDelete}]
%%
%%---------------------------------------------------------------
-spec actual_state()-> [term()].
actual_state()->
    gen_server:call(?SERVER, {actual_state},infinity).

%%---------------------------------------------------------------
%% Function: load_read_specs()
%% @doc: git load and read service_specs files        
%% @param: non
%% @returns:[ServiceSpecsInfo]|{error,Reason}
%%
%%---------------------------------------------------------------
-spec load_read_specs()-> [term()]|{atom(),term()}.
load_read_specs()->
    gen_server:call(?SERVER, {load_read_specs},infinity).

%%---------------------------------------------------------------
%% Function:template()
%% @doc: service spec template  list of {app,vsn} to run      
%% @param: 
%% @returns:[{app,vsn}]
%%
%%---------------------------------------------------------------
%-spec template()-> [{atom(),string()}].
%template()->
 %   gen_server:call(?SERVER, {template},infinity).


%% ====================================================================
%% Support functions
%% ====================================================================
%%---------------------------------------------------------------
%% Function:all_specs()
%% @doc: all service specs infromation       
%% @param: non 
%% @returns:State#state.service_specs_info
%%
%%---------------------------------------------------------------
-spec all_specs()-> [term()].
all_specs()->
    gen_server:call(?SERVER, {all_specs},infinity).

%%---------------------------------------------------------------
%% Function:read_state()
%% @doc: read theServer State variable      
%% @param: non 
%% @returns:State
%%
%%---------------------------------------------------------------
-spec read_state()-> term().
read_state()->
    gen_server:call(?SERVER, {read_state},infinity).
%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
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
  
    io:format("common ~p~n",[application:start(common)]),
    io:format("nodelog ~p~n",[application:start(nodelog)]),
    nodelog_server:create(?LogDir),
    io:format("sd_app ~p~n",[application:start(sd_app)]),
    io:format("config ~p~n",[application:start(config_app)]),
    Info=lib_appl:service_specs_info(),
    ok= nodelog_server:log(notice,?MODULE_STRING,?LINE,"server started"),

    {ok, #state{ vm_list=[],
		 service_specs_info=Info}
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

handle_call({create_vm},_From, State) ->
    UniqueNodeName=integer_to_list(erlang:system_time(microsecond),36),
    Reply=case lib_vm:create(UniqueNodeName) of
	      {ok,Vm}->
		  case {rpc:call(Vm,code,add_patha,["ebin"],5000),rpc:call(Vm,code,add_patha,[?ControllerEbin],5000)} of
		      {{error,_},{error,_}}->
			  rpc:call(Vm,init,stop,[],5000),
			  NewState=State,
			  {error,bad_directory};
		      _->
			  case rpc:call(Vm,application,start,[nodelog],5000) of
			      ok->
				  rpc:call(Vm,nodelog_server,create,[?LogDir],5000),
				  rpc:call(Vm,nodelog_server,log,[notice,?MODULE_STRING,?LINE,"slave started "++atom_to_list(Vm)],5000),
				  nodelog_server:log(notice,?MODULE_STRING,?LINE,"slave started "++atom_to_list(Vm)),
				  case rpc:call(Vm,application,start,[service_app],5000) of
				      ok->
					  NewState=State#state{vm_list=[Vm|State#state.vm_list]},
					  {ok,Vm};
				      {error,_Reason}->
					  rpc:call(Vm,init,stop,[],5000),
					  NewState=State
				  end;
			      {error,_Reason}->
				  rpc:call(Vm,init,stop,[],5000),
				  NewState=State
			  end			 
		  end
	  end,
    {reply, Reply, NewState};

handle_call({delete_vm,Vm},_From, State) ->
    Reply=case lib_vm:delete(Vm) of
	      ok->
		  NewState=State#state{vm_list=lists:delete(Vm,State#state.vm_list)},
		  ok;
	      {error,Reason}->
		  NewState=State,
		  {error,Reason}
	  end,
    {reply, Reply, NewState};

handle_call({load_start_appl,ApplId,ApplVsn,Node},_From, State) ->
    Reply=case lists:member(Node,State#state.vm_list) of
	      false->
		  {error,[eexists,Node]};
	      true->
		 % case lists:keyfind({ApplId,ApplVsn},1,State#state.service_specs_info) of
		  case config:find(ApplId,ApplVsn) of
		      false->
			  {error,[eexists,ApplId,ApplVsn]};
		      {ApplId,_VsnList,GitPath}->
			  case rpc:call(Node,service,load,[ApplId,ApplVsn,GitPath],20*5000) of
			      {error,Reason}->
				  {error,Reason};
			      ok ->
				 rpc:call(Node,service,start,[ApplId,ApplVsn],20*5000) 
			  end	
		  end	  
	  end,
    {reply, Reply, State};

handle_call({stop_unload_appl,ApplId,ApplVsn,Node},_From, State) ->
    Reply=case lists:member(Node,State#state.vm_list) of
	      false->
		  {error,[eexists,Node]};
	      true->
		  case rpc:call(Node,service,stop,[ApplId,ApplVsn],5000) of
		      {error,Reason}->
			  {error,Reason};
		      ok ->
			  rpc:call(Node,service,unload,[ApplId,ApplVsn],5000) 
		  end	
	  end,
    {reply, Reply, State};


handle_call({all_specs},_From, State) ->
    Reply=State#state.service_specs_info,
    {reply, Reply, State};

handle_call({get_spec,Name,Vsn},_From, State) ->
    Reply=lib_appl:get_spec(Name,Vsn,State#state.service_specs_info),
    {reply, Reply, State};

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

		  

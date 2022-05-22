%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_appl).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).
-export([
%	 load_start/4,
%	 stop_unload/3,
	 service_specs_info/0
	]).
	 

%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
service_specs_info()->
    AllDeploymentInfo=config:all_info(),       
    deploy_info(AllDeploymentInfo,[]).


deploy_info([],DeployInfo)->
    DeployInfo;
deploy_info([{_DeplId,_DeplVsn,ApplId,ApplVsn,[all]}|T],Acc)->
%    io:format(" ~p~n",[{{DeplId,DeplVsn,ApplId,ApplVsn,[all]},?MODULE,?LINE}]),
    NewAcc=case config:find(ApplId,ApplVsn) of
	       false->
		   Acc;
	       {AppId,_VsnList,GitPath}->
		   [{{AppId,ApplVsn},GitPath}|Acc]
	   end,
    deploy_info(T,NewAcc);

deploy_info([{_DeplId,_DeplVsn,ApplId,ApplVsn,AffinityList}|T],Acc)->
%      io:format("ApplId,ApplVsn,AffinityList ~p~n",[{ApplId,ApplVsn,AffinityList,?MODULE,?LINE}]),
    NewAcc= case lists:member(node(),AffinityList) of
		false->
%		    io:format("node not member~p~n",[{node(),AffinityList,?MODULE,?LINE}]),
		    Acc;
		true->
		    case config:find(ApplId,ApplVsn) of
			false->
			   % io:format("config not member~p~n",[{?MODULE,?LINE}]),
			    Acc;
			{AppId,_VsnList,GitPath}->
			   % io:format("Ok ~p~n",[{AppId,GitPath,?MODULE,?LINE}]),
			    [{{AppId,ApplVsn},GitPath}|Acc]
		    end
	    end,
    deploy_info(T,NewAcc).    
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
% NodeDir=filename:join(NodeName++".pod"),

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
 

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

    



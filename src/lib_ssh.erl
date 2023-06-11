%% @author Paolo Oliveira <paolo@fisica.ufc.br>
%% @copyright 2015-2016 Paolo Oliveira (license MIT)
%% @version 1.0.0
%% @doc
%% A simple, pure erlang implementation of a module for <b>Raspberry Pi's General Purpose
%% Input/Output</b> (GPIO), using the standard Linux kernel interface for user-space, sysfs,
%% available at <b>/sys/class/gpio/</b>.
%% @end
 
-module(lib_ssh).
-export([send/6]).
-author('joq erlang').

-define(DELAY,2000).
% io:format("Reply ~p~n",[{Reply,?MODULE,?FUNCTION_NAME,?LINE}]),

% Results:
% {ok,[]} -> successful linux command returned status zero and no info
% {ok,Term} -> successful linux command returned status zero and info Term
%% {error,Reason} -> failed linux comman returned status 1 and error message Reason

send(Ip,Port,User,Password,Msg,TimeOut)->
    Reply=case ssh_connect(Ip,Port,User,Password,TimeOut) of
	      {error,Err}->
		  {error,[Err,?MODULE,?FUNCTION_NAME,?LINE]};
	      {ok,ConRef,ChanId}->
		  ssh_connection:exec(ConRef,ChanId,Msg,TimeOut),
		  Result=undefined,
		  Data=[],
		  Closed=false,
		  SessionResult=rec(ConRef,ChanId,Result,Data,Closed,TimeOut),
		  ssh:close(ConRef),
		  SessionResult;
	      Reason ->
		  {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]}
	  end,
    Reply.


ssh_connect(Ip,Port,User,Password,TimeOut)->
    Result=case ssh:connect(Ip,Port,[{user,User},{password,Password},
				     {silently_accept_hosts, true} ],TimeOut) of
	       {error,Err}->
		   {error,Err};
	       {ok,ConRef}->
		   case ssh_connection:session_channel(ConRef,TimeOut) of
		       {error,Err}->
			   {error,[Err,?MODULE,?FUNCTION_NAME,?LINE]};
		       {ok,ChanId}->
			   {ok,ConRef,ChanId}
		   end;
	       Err2 ->
		   {error,[Err2]}
	   end,
    Result.

rec(_ConRef,_ChanId,Result,Data,true,_TimeOut)->
    {Result,Data};
%[{error,[["rm: cannot remove 'glurk'"],lib_ssh,rec,68]},{error,[[": No such file or directory"],lib_ssh,rec,68]}]

rec(ConRef,ChanId,Result,Data,Closed,TimeOut)->
%% Assumption either data is ok or error. Data can be sent in number of frames till eof is present
    receive
	{ssh_cm, ConRef, {data, ChanId, Type, RecData}} when Type == 0 ->
	    NewClosed=false,
	    X1=binary_to_list(RecData),
	    ParsedData=string:tokens(X1,"\n"),
	    NewResult=ok,
	    NewData=[ParsedData|Data],
%	    io:format("OK: NewResult,NewData,Closed ~p~n",[{NewResult,NewData,Closed,?MODULE,?FUNCTION_NAME,?LINE}]),
	    ok;
	{ssh_cm, ConRef, {data, ChanId, Type, RecData}} when Type == 1 ->
	    %io:format("Error: RecData ~p~n",[{RecData,?MODULE,?FUNCTION_NAME,?LINE}]),
	    NewClosed=false,
	    X1=binary_to_list(RecData),
	    %io:format("Error: X1 ~p~n",[{X1,?MODULE,?FUNCTION_NAME,?LINE}]),
	    ParsedData=string:tokens(X1,"\n"),
	    NewResult=error,
	    NewData=[ParsedData|Data],
	    %io:format("Error: NewResult,NewData,Closed ~p~n",[{NewResult,NewData,Closed,?MODULE,?FUNCTION_NAME,?LINE}]),
	    ok;	  
	{ssh_cm,ConRef,{eof,0}} ->
	    if 
		Result == undefined->
		    NewResult=ok;
	       true->
		    NewResult=Result
	    end,
	    NewClosed=false,
	    NewData=Data,
	    %io:format("{eof,0} NewResult,NewData,Closed ~p~n",[{NewResult,NewData,Closed,?MODULE,?FUNCTION_NAME,?LINE}]),
	    ok;
	{ssh_cm,ConRef,{exit_status,0,0}} ->
	    NewClosed=false,
	    NewResult=Result,
	    NewData=Data,
	     %io:format("{exit_status,0,0}: NewResult,NewData,Closed ~p~n",[{NewResult,NewData,Closed,?MODULE,?FUNCTION_NAME,?LINE}]),
	    ok;
	{ssh_cm,ConRef,{exit_status,0,1}} ->
	    NewClosed=false,
	    NewResult=Result,
	    NewData=Data,
             %io:format("{exit_status,0,1}: NewResult,NewData,Closed ~p~n",[{NewResult,NewData,Closed,?MODULE,?FUNCTION_NAME,?LINE}]),
	    ok;
	{ssh_cm,ConRef,{closed,0}} ->
	    NewClosed=true,
	    NewResult=Result,
	    NewData=lists:reverse(lists:append(Data)),
	      %io:format("{closed,0}: NewResult,NewData,Closed ~p~n",[{NewResult,NewData,Closed,?MODULE,?FUNCTION_NAME,?LINE}]),
	    ok;
	Unmatched->
	    NewClosed=true,
	    NewResult=error,
	    NewData=[{"Unmatched",Unmatched,?MODULE,?LINE}|Data],
	     %io:format("OK: NewResult,NewData,Closed ~p~n",[{NewResult,NewData,Closed,?MODULE,?FUNCTION_NAME,?LINE}]),
	    ok
    after TimeOut->
	    NewClosed=true,
	    NewResult=error,
	    NewData=[{"Timeout ",?MODULE,?LINE}|Data]
    end,
    rec(ConRef,ChanId,NewResult,NewData,NewClosed,TimeOut).

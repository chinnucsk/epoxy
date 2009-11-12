%%% ----------------------------------------------------------------------
%%% Author  : nickhristov
%%% Description : A simple generic server that binds to a address and port
%%%               and spawns X children to handle the tcp connections.
%%% Created : Nov 11, 2009
%%% ----------------------------------------------------------------------
-module(listen_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-import(gen_tcp).
-import(channel).
-record(state, {ipaddress, port, socket}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
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
    {ok, #state{}}.

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

%% LISTEN:
handle_call({listen, 0}, From, State) -> % base case:
	{reply, {ok}, State};

handle_call({listen, NumChildren}, From, #state{ipaddress=IpAddress, port=Port, socket=ListenSocket}) ->
	spawn_link( fun() -> channel:start_channel(ListenSocket) end),
	handle_call({listen, NumChildren -1}, From, #state{ipaddress=IpAddress, port=Port, socket=ListenSocket});

%% START:
handle_call({start,StartRequest}, From, #state{}) -> 
	{IpAddress, PortNumber} = StartRequest,
	ListenSocket = gen_tcp:listen(PortNumber, { }),
	NewState = #state{ipaddress=IpAddress, port=PortNumber, socket=ListenSocket},	
	{reply, {ok}, NewState};
handle_call({start,StartRequest}, From, #state{ipaddress=_, port=_, socket=ListenSocket}) ->
	disconnect(ListenSocket),
	handle_call(StartRequest, From, #state{}).


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

disconnect(Socket) ->
	gen_tcp:close(Socket).

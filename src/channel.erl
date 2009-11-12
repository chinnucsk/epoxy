%% Author: nickhristov
%% Created: Nov 11, 2009
%% Description: TODO: Add description to channel
-module(channel).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([pipe_channel/2]).

%%
%% API Functions
%%

pipe_channel(SocketA, SocketB) -> 
	receive
		{tcp, SocketA, Data} ->
			gen_tcp:send(SocketB, Data),
			pipe_channel(SocketA, SocketB);
		{tcp, SocketB, Data} ->
			gen_tcp:send(SocketA, Data),
			pipe_channel(SocketA, SocketB);
		{tcp_closed, _} -> {ok}
	end.

%%
%% Local Functions
%%


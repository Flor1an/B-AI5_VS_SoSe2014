%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Mai 2014 15:45
%%%-------------------------------------------------------------------
-module(client).
-author("Florian").

%% API
-export([con/1,getAll/0,send/0,getNum/0]).

con(Servername)->
  net_adm:ping(Servername).


% Anfrage der Clients
%rpc(Req) ->
% Nachschlagen der Prozessid des Servers
%  Server = global:whereis_name(theserver),
% Kontrollausgabe
%  io:format("Server PID ~p~n", [Server]),
% request versenden
%  Server ! {self(),{request,Req}},
% Antwort emfangen und ausgeben
%  receive {theserver,{reply,Rep}} ->
%      io:format("Reply ~w~n", [Rep]);
%    Any ->
%      io:format("Illegal Response ~w~n", [Any])
%  end.

getAll() ->
  Server = global:whereis_name(theserver),
  io:format("Server PID ~p~n", [Server]),

  Server ! { query_messages, self()},
  receive { message, Number,Nachricht,Terminated} ->
      io:format("~w : ~s : ~w~n", [Number,Nachricht,Terminated])
  end.

send() ->
  Server = global:whereis_name(theserver),
  io:format("Server PID ~p~n", [Server]),

  Nachricht="Test Nachricht",
Number =0000,%TEMP

  Server ! {new_message, {Nachricht, Number}},
  io:format(ok).


getNum() ->
  Server = global:whereis_name(theserver),
  io:format("Server PID ~p~n", [Server]),
  Server ! { query_msgid, self()},
  receive
    { msgid, Number} ->
      io:format("Got Number ~w from Server~n", [Number])
  end.



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

-import(werkzeug, [timeMilliSecond/0]).


%% API
-export([con/1,start/0]).

con(Servername)->
  net_adm:ping(Servername).

start()-> %%REDAKTEUR!!
  Number = getNum(),%Nummer organisieren
  %TODO: TIMER required
  send(Number). %Nahricht mit der Nummer schicken


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

getNum() ->
  Server = global:whereis_name(theserver),
  io:format("Server PID ~p~n", [Server]),
  Server ! { query_msgid, self()},
  receive
    { msgid, Number} ->
      io:format("Got Number ~w from Server~n", [Number]),
      Number %Returnvalue
  end.

send(Number) ->
  Server = global:whereis_name(theserver),
  io:format("Number ~w to Server PID ~p~n", [Number,Server]),

  EigenenNamen="client",
  RechnerName="@FloUB",
  ProzessNummer="23456",
  PraktikumsGruppe="02",
  TeamNummer="03",
  AktuelleSystemzeit=timeMilliSecond(),


  Nachricht=string:join([EigenenNamen,RechnerName,ProzessNummer,PraktikumsGruppe,TeamNummer,AktuelleSystemzeit],"-"),
  %TODO: Drop one out of six
  Server ! {new_message, {Nachricht, Number}},
  io:format(ok).







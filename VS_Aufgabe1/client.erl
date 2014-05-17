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
  %Drop one out of six
  case Number rem 5 of
    0 ->
      %Vergessen
      io:format("NO MESSAGE SEND DUE TO RESTRICTION BY MISSION!~n");
    _ ->
      %Senden
      Server = global:whereis_name(theserver),

      EigenenNamen="client",
      RechnerName="@FloUB",
      ProzessNummer=io_lib:format("~p", [Server]),
      PraktikumsGruppe="02",
      TeamNummer="03",
      AktuelleSystemzeit=werkzeug:timeMilliSecond(),


      Nachricht=string:join([EigenenNamen,RechnerName,ProzessNummer,PraktikumsGruppe,TeamNummer,AktuelleSystemzeit],"-"),
      io:format("SENDING: ~p ~s~n", [Number, Nachricht]),


      Server ! {new_message, {Nachricht, Number}},
      io:format(ok)
end.








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
-export([con/1,redakteur/0,leser/0,getNum/0]).

con(Servername)->
  net_adm:ping(Servername).

%%REDAKTEUR!######################################################
redakteur()->
  Number = getNum(),%Nummer organisieren
  %TODO: TIMER required
  timer:sleep(1000),
  send(Number). %Nahricht mit der Nummer schicken




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
      EigenenNamen="bla",
      RechnerName=net_adm:localhost(),
      ProzessNummer=io_lib:format("~p", [Server]),
      PraktikumsGruppe="02",
      TeamNummer="03",
      AktuelleSystemzeit=werkzeug:timeMilliSecond(),


      Nachricht=string:join([EigenenNamen,"@",RechnerName,ProzessNummer,PraktikumsGruppe,TeamNummer,AktuelleSystemzeit],"-"),
      io:format("SENDING: ~p ~s~n", [Number, Nachricht]),


      Server ! {new_message, {Nachricht, Number}},
      io:format(ok)
end.


%%LESER!##################################################
leser() ->
  getNext().


getNext() ->
  Server = global:whereis_name(theserver),
  io:format("Server PID ~p~n", [Server]),

  Server ! { query_messages, self()},
  receive

    { message, Number,Nachricht,true} -> %%Keine weiteren Nachrichten auf Server
      io:format(": ~w : ~s : ~w~n", [Number,Nachricht,true]);

    { message, Number,Nachricht,false} -> %% NOCH weitere NAchrichten auf Server
      io:format(": ~w : ~s : ~w~n", [Number,Nachricht,false]),
      getNext()
  end.







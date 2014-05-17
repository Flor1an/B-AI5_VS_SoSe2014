%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Mai 2014 15:42
%%%-------------------------------------------------------------------
-module(myserver).
-author("Florian").

%% API
-export([start/0,server/1,fn/3]).

-record(status, {
  current_msg_id  = 0
}).


% Server starten und unter Namen "theserver" global registrieren
% Alternative: Servername aus config Datei lesen
start() ->
% {ok, ConfigListe} = file:consult("server.cfg"),
% {ok, Lifetime} = werkzeug:get_config_value(lifetime, ConfigListe),
% {ok, HostName} = inet:gethostname(),
% Datei = lists:concat(["NServer@",HostName,".log"]),


  State = #status{ current_msg_id = 0},

  Pid = spawn(myserver,server,[State]),
  global:register_name(theserver,Pid).



% Verarbeiten der Daten und Antwort Ergebnis
% von fn(...) an Client senden
server(Status) ->
  receive

    {query_messages, From} -> %Abfragen aller Nachrichten (Nacheinander)
      Return = query_messages(),

      {MsgId,Nachricht,Terminated} = Return, %Aufsplitten

      From ! {message, MsgId,Nachricht,Terminated},% SIGNATURE: message,Number,Nachricht,Terminated
      server(Status);

    {new_message, {Nachricht,Number}} -> %Senden einer Nachricht
      new_message({Nachricht,Number}),
      %NO REPLY REQUIRED
      server(Status);

    {query_msgid, From} -> %Request current number for message
      Return = query_msgid(Status),
      Number = Return#status.current_msg_id,
      From ! {msgid,Number}, %SIGNATURE:  msgid, Number
      server(Return)

  end.

query_messages() ->
  io:format("query_messages METHODE~n"),
  FakeID = 1,
  FakeMessage = "Blabla Das ist eine NAchricht",
  FakeBool = true,
  {FakeID,FakeMessage,FakeBool}.

new_message({Nachricht,Number}) ->
  io:format("new_message METHODE with: (~w) ~s ~n",[Number,Nachricht]).

query_msgid(State) ->
  io:format("query_msgid METHODE~n"),
  NewState = State#status{current_msg_id = State#status.current_msg_id + 1},
  NewState.





fn(From,X,Data) ->
% Gibt den Inhalt von Data als Paare Prozessid Datum aus
  Data1 = [[From,X]|Data],
  io:format("~p~n",
    [lists:flatten([io_lib:format("~p:~w ",Tupel ) || Tupel <- Data1])]),
% Ergebnis
  {[X,X],Data1}.

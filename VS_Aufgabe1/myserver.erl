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
-export([start/0,server/1]).

-record(status, {
  current_msg_id  = 0,
  hbq =werkzeug:emptySL(),
  dlq = werkzeug:emptySL(),
  clients=werkzeug:emptySL(),
  config
}).

-record(config,{
  latency,
  clientlifetime,
  servername,
  dlqlimit
}).

load_config() ->
  {ok, ConfigFile} = file:consult('server.cfg'),
  #config{
    latency = proplists:get_value(latency, ConfigFile),
    clientlifetime = proplists:get_value(clientlifetime, ConfigFile),
    servername = proplists:get_value(servername, ConfigFile),
    dlqlimit = proplists:get_value(dlqlimit, ConfigFile)
  }.


% Server starten und unter Namen "theserver" global registrieren
% Alternative: Servername aus config Datei lesen
start() ->
  Config = load_config(),
  State = #status{ config = Config },
  ServerPID = spawn(fun() -> server(State) end),
  io:format("ServerPID=~p~n",[ServerPID]),
  register(Config#config.servername, ServerPID),
  ServerPID.



% Verarbeiten der Daten und Antwort Ergebnis
% von fn(...) an Client senden
server(Status) ->
  receive

    {query_messages, From} -> %Abfragen aller Nachrichten (d.h. pro Client höchste, noch nicht übermittelte Nachricht)
      Return = query_messages(Status,From),

      {NewStatus,{MsgId,Nachricht,Terminated}} = Return, %Aufsplitten

      From ! {message, MsgId,Nachricht,Terminated},% SIGNATUR: {keyword message,Number,Nachricht,TerminatedFlag}
      server(NewStatus);

    {new_message, {Nachricht,Number}} -> %Empfangen einer Nachricht
      Return = new_message(Status,Number,Nachricht),
      %NO REPLY REQUIRED
      server(Return);

    {query_msgid, From} -> %Abfragen nach neuer (höchste vergebene +1) MessageID
      Return = query_msgid(Status),
      Number = Return#status.current_msg_id,
      From ! {msgid,Number}, %SIGNATUR:  {keyword msgid, Number}
      server(Return);

    Else ->
      io:format("Cannot handle: ~p", [Else])

  end.

%% #####################################################################################################################

%% Gibt die naechste freie ID an einen beliebigen Client.
query_msgid(State) ->
  io:format("[query_msgid METHODE]~n"),
  NewState = State#status{current_msg_id = State#status.current_msg_id + 1},
  NewState.

%% #####################################################################################################################

%% Empfaengt eine neue Nachricht und Speichert sie in der Holdbackqueue.
new_message(State,Number,Nachricht) ->
  io:format("[new_message METHODE]~n"),
  io:format("Recived: (~w) ~s ~n",[Number,Nachricht]),
  NachrichtAndTimeStamp = string:join([Nachricht, "|To HBQ:", werkzeug:timeMilliSecond()], " "),

  NewState = State#status{hbq = werkzeug:pushSL(State#status.hbq,{Number,NachrichtAndTimeStamp})},
  %NewState = State#status{hbq = lists:append(State#status.hbq, {Nachricht})},
  io:format("[NEW LIST]~p~n",[NewState#status.hbq]),
  NewState.

%% #####################################################################################################################

%% Liefert Nachrichten an einen anfragenden Client
query_messages(State,ClientPID) ->
  io:format("~n~n[query_messages METHODE]~n~n"),
  NewState = moveMessagesFromHbqToDlq(State),
  io:format("~n~n[back in query_messages METHODE]~n~n"),
  io:format("[DLQ] ~p~n",[werkzeug:lengthSL(NewState#status.dlq)]),
  io:format("[HBQ] ~p~n",[werkzeug:lengthSL(NewState#status.hbq)]),

  io:format("[werkzeug:findSL(NewState#status.clients,ClientPID)] ~p~n",[werkzeug:findSL(NewState#status.clients,ClientPID)]),


  case werkzeug:findSL(NewState#status.clients,ClientPID) of

    {-1,nok} -> %Nicht vorhanden? Initial anlegen
      io:format("[CLIENT NOCH NCIHT VORHANDEN (wird angelegt)] ~n"),
      %io:format("[dlq] ~p~n",[NewState#status.dlq]),
      NewClientTupel = {ClientPID,1}, %Initialisiert mit 1
      TempStatus= NewState#status{clients = werkzeug:pushSL(NewState#status.clients,NewClientTupel)},
      io:format("[ClientList]: ~p~n",[TempStatus#status.clients]),
      query_messages(TempStatus,ClientPID);

    {_,RequestedIDFromClient} -> %Client in Liste vorhanden
      io:format("[CLIENT VORHANDEN] ~n"),
      io:format("[Client will ID] ~p ~n",[RequestedIDFromClient]),
      io:format("[DLQ] ~p ~n",[NewState#status.dlq]),


      {TempStatus,RealIDToTransfer,RealMessageToTransfer} = case werkzeug:findneSL(NewState#status.dlq,RequestedIDFromClient) of
                                                             {-1,nok} -> % ID nicht vorhanden also dummy sendne
                                                                 T2 = "DUMMY MESSAGE (keine neuen Nachrichten vorhanden)",
                                                                 T1 = NewState,
                                                                 {T1,RequestedIDFromClient,T2}; %Return

                                                             {IDToTransfer, MessageToTransfer} -> %ID vorhanden IDToTransfer ggf. <= RequestedIDFromClient
                                                                 {T1,T2,T3} = case MessageToTransfer of
                                                                            {Message,_,_} -> %Gap Message
                                                                                T22 = Message,
                                                                                T11 =changeClientNextMessageIdTo(IDToTransfer+1,ClientPID,NewState),  % increment counter
                                                                                {T11,IDToTransfer,T22};
                                                                            Message -> %Normal Message
                                                                                T22 = Message,
                                                                                T11 =changeClientNextMessageIdTo(IDToTransfer+1,ClientPID,NewState),% increment counter
                                                                                {T11,IDToTransfer,T22}
                                                                  end,

                                                                  {T1,T2,T3}%Return
                                                           end,

      RealBoolToTransfer = case RealIDToTransfer >= werkzeug:maxNrSL(TempStatus#status.dlq) of
                            true -> true;
                            false -> false %auf True setzten für einzeln abfragen
                           end,

      %Return
      {TempStatus,{RealIDToTransfer,RealMessageToTransfer,RealBoolToTransfer}}

  end.





%% Uebertraegt Nachrichten von der Holdbackqueue in die Deliveryqueue.
%% Fuellt Luecken mit einer Fehlernachricht.
moveMessagesFromHbqToDlq(State) ->

  Config = State#status.config,

  NewState = case werkzeug:maxNrSL(State#status.dlq) < Config#config.dlqlimit of
    true -> %In der Deliveryqueue sind noch Plaetze frei:

      case werkzeug:lengthSL(State#status.hbq) >0 of
        true -> %Es sind noch Nachrichten in der Holdbackqueue (die in die Deleiveryqueue uebertragen werden koennen)
          LowestIDInHBQ = werkzeug:minNrSL(State#status.hbq),
          io:format("[KeyToMove from HBQ to DLQ (LowestIDInHBQ)] ~p~n",[LowestIDInHBQ]),
          HighestIDinDLQ = maxNrSLHelper(State#status.dlq, State),
          io:format("[HighestIDinDLQ] ~p~n",[HighestIDinDLQ]),



          Temp3State = case HighestIDinDLQ+1 < LowestIDInHBQ  of %wenn Luecke (von min 1 elem)
            true -> %Der naechste Eintrag in der Holdbackqueue ist groesser, als der letzte in der Deliveryqueue; Also haben wir eine Luecke die gefuellt werden muss!
              io:format("[ID is NOT present in HBQ]~n"),
              MessageAndTimestamp ={string:join(["****Fehlernachricht (Lueckenfueller) fuer Nachricht", werkzeug:to_String(HighestIDinDLQ+1),"bis", werkzeug:to_String(LowestIDInHBQ-1),"um ", werkzeug:timeMilliSecond()]," "),HighestIDinDLQ+1,LowestIDInHBQ-1},

              TempState =State#status{dlq = werkzeug:pushSL(State#status.dlq, {HighestIDinDLQ+1,MessageAndTimestamp})}, %Speicher PlainText Nachricht + Lueckenstart + Lueckenende Werte in der Deliveryqueue
              TempState;


            false -> %Der naechste Eintrag in der Holdbackqueue hat die MessageID, die in der Deliveryqueue erwarted wird (keine Lücke erkannt)
              io:format("[Id is in Row / ID is present in HBQ (all good)]~n"),
              {_,TempMessage} = werkzeug:findSL(State#status.hbq, LowestIDInHBQ),%Kopier naechste Nachricht aus der Holdbackqueue in Variable
              MessageAndTimestamp = string:join([TempMessage,"|To DLQ:",werkzeug:timeMilliSecond()]," "),

              TempState = State#status{dlq = werkzeug:pushSL(State#status.dlq, {LowestIDInHBQ,MessageAndTimestamp})}, %Speicher die Nachricht in der Deliveryqueue
              Temp2State = TempState#status{hbq = werkzeug:popSL(TempState#status.hbq)}, %Entferne die (jetzt alte) Nachricht aus der Holdbackqueue popSL=kleinste nummer
              Temp2State

          end,


          io:format("State#status.dlq: ~p~n", [Temp3State#status.dlq]),
          io:format("State#status.hbq: ~p~n", [Temp3State#status.hbq]),
          moveMessagesFromHbqToDlq(Temp3State); %Recusiver Aufruf, bis Deliveryqueue maximal gefuellt (oder keine Nachrichten mehr in der Holdbackqueue zum verschieben vorhanden)


      _ -> %Die Holdbackqueue hat (z.Zt.) keine Nachrichten mehr.
        State
      end;
    _ -> %In der Deliveryeue sind alle Plaetze belegt: d.h. nichts kopieren.
      State
  end,
  NewState.


%% #############################################HELPER##################################################################

%% Setzt einen neuen Wert für den Lese Client
changeClientNextMessageIdTo(NewValue,ClientPID,State)->
  {ClientPID,CurrentValue} = werkzeug:findSL(State#status.clients,ClientPID),

  NewState =State#status{clients = lists:delete({ClientPID,CurrentValue},State#status.clients)}, %Altes Value loeschen
  Return = NewState#status{clients = werkzeug:pushSL(NewState#status.clients,{ClientPID,NewValue})}, %Return State mit neuem Value für Client
  io:format("[changeClientNextMessageIdTo() return]~p~n",[Return]),
  Return.


%% Helfermethode zum ermitteln der hoechsten Id der Deliveryqueue
maxNrSLHelper(Liste,State)->
  case werkzeug:maxNrSL(Liste) of
    -1 -> 0; %Bei Leerer Liste
    _ ->

      T = werkzeug:maxNrSL(Liste),
      Return = case werkzeug:findSL(State#status.dlq,T) of
                 {_,{_,_,To}} -> %Wenn hoechster = Lueckenfueller
                   To; % dann geb ID wo die Luecke zuende ist zurueck
                 _ ->
                   T %sonst geb MaxId zurueck
               end,
      Return
  end.

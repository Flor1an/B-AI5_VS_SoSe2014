-module(myserver).

-export([start/0]).

-record(status, {
  current_msg_id  = 0,
  hbq =werkzeug:emptySL(),
  dlq = werkzeug:emptySL(),
  clients=werkzeug:emptySL()
  
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


start() ->
  Config = load_config(),
  State = #status{  },
  ServerPID = spawn_link(fun() -> server(State,Config) end),
  io:format("ServerPID=~p~n",[ServerPID]),
  register(Config#config.servername, ServerPID),
  ServerPID.



server(Status,Config) ->
    {ok,MyTimer} = timer:send_after(timer:seconds(Config#config.latency), self(), "Ende Gelaende"),

  receive

    {query_messages, From} -> %Abfragen aller Nachrichten (d.h. pro Client höchste, noch nicht übermittelte Nachricht)
      Return = query_messages(Status,Config,From),

      {NewStatus,{MsgId,Nachricht,Terminated}} = Return, %Aufsplitten

      From ! {message, MsgId,Nachricht,Terminated},% SIGNATUR: {keyword message,Number,Nachricht,TerminatedFlag}
	  timer:cancel(MyTimer),
      server(NewStatus,Config);

    {new_message, {Nachricht,Number}} -> %Empfangen einer Nachricht
	  log("Server","<< : Empfange Nachricht ~p) ~s", [Number,Nachricht]),
      Return = new_message(Status,Number,Nachricht),
      %NO REPLY REQUIRED
	  timer:cancel(MyTimer),
      server(Return,Config);

    {query_msgid, From} -> %Abfragen nach neuer (höchste vergebene +1) MessageID
	  log("Server","~p << Nummer anfragen", [From]),
      Return = query_msgid(Status),
      Number = Return#status.current_msg_id,
	  log("Server","~p >> : Sende Nummer ~p", [From,Number]),
      From ! {msgid,Number}, %SIGNATUR:  {keyword msgid, Number}
	  timer:cancel(MyTimer),
      server(Return,Config);

    Else ->
      io:format("Cannot handle: ~p", [Else])

  end.

%% 1 #####################################################################################################################
%% Gibt die naechste freie ID an einen beliebigen Client.
query_msgid(State) ->
  NewState = State#status{current_msg_id = State#status.current_msg_id + 1},
  NewState.

  
  
%% 2 #####################################################################################################################
%% Empfaengt eine neue Nachricht und Speichert sie in der Holdbackqueue.
new_message(State,Number,Nachricht) ->
  NachrichtAndTimeStamp = string:join([Nachricht, "|To HBQ:", werkzeug:timeMilliSecond()], " "),
  NewState = State#status{hbq = werkzeug:pushSL(State#status.hbq,{Number,NachrichtAndTimeStamp})},
  NewState.

  
  
%% 3 #####################################################################################################################
%% Liefert Nachrichten an einen anfragenden Client
query_messages(State,Config,ClientPID) ->
  StateWithUpdatedQueues = moveMessagesFromHbqToDlq(State,Config),
  NewState = deleteOldClients(StateWithUpdatedQueues,Config),
  
  case werkzeug:findSL(NewState#status.clients,ClientPID) of

    {-1,nok} -> %Nicht vorhanden? Initial anlegen
      log("Server","~p || Wird als neuer Client gespeichert", [ClientPID]),
      NewClientTupel = {ClientPID,{1,erlang:now()}}, %Initialisiert mit MessageID 1
      TempStatus= NewState#status{clients = werkzeug:pushSL(NewState#status.clients,NewClientTupel)},
      query_messages(TempStatus,Config,ClientPID);

    {_,{RequestedIDFromClient,_}} -> %Client in Liste vorhanden
	  log("Server","~p || Ist als Client bekannt. Nachrichten werden uebertragen", [ClientPID]),	  
	  NewNewState = updateClientRequestTime(NewState,ClientPID),
	  
      {TempStatus,RealIDToTransfer,RealMessageToTransfer} = case werkzeug:findneSL(NewNewState#status.dlq,RequestedIDFromClient) of
                                                             {-1,nok} -> % ID nicht vorhanden also dummy sendne
                                                                 T2 = "DUMMY MESSAGE (keine neuen Nachrichten vorhanden)",
                                                                 T1 = NewState,
                                                                 {T1,RequestedIDFromClient,T2}; %Return

                                                             {IDToTransfer, MessageToTransfer} -> %ID vorhanden IDToTransfer ggf. <= RequestedIDFromClient
                                                                 {T1,T2,T3} = case MessageToTransfer of
                                                                            {Message,_,_} -> %Gap Message
                                                                                T22 = Message,
                                                                                T11 =changeClientNextMessageIdTo(IDToTransfer+1,ClientPID,NewNewState),  % increment counter
                                                                                {T11,IDToTransfer,T22};
                                                                            Message -> %Normal Message
                                                                                T22 = Message,
                                                                                T11 =changeClientNextMessageIdTo(IDToTransfer+1,ClientPID,NewNewState),% increment counter
                                                                                {T11,IDToTransfer,T22}
                                                                  end,

                                                                  {T1,T2,T3}%Return
                                                           end,

      RealBoolToTransfer = case RealIDToTransfer >= werkzeug:maxNrSL(TempStatus#status.dlq) of
                            true -> true;
                            false -> false %auf True setzten für einzeln abfragen
                           end,
		
		
	  log("Server","~p >> bekommt ~p) ~s [~p]", [ClientPID,RealIDToTransfer,RealMessageToTransfer,RealBoolToTransfer]),
      %Return
      {TempStatus,{RealIDToTransfer,RealMessageToTransfer,RealBoolToTransfer}}

  end.





%% Uebertraegt Nachrichten von der Holdbackqueue in die Deliveryqueue.
%% Fuellt Luecken mit einer Fehlernachricht.
moveMessagesFromHbqToDlq(State,Config) ->


  NewState = case werkzeug:maxNrSL(State#status.dlq) < Config#config.dlqlimit of
    true -> %In der Deliveryqueue sind noch Plaetze frei:

      case werkzeug:lengthSL(State#status.hbq) >0 of
        true -> %Es sind noch Nachrichten in der Holdbackqueue (die in die Deleiveryqueue uebertragen werden koennen)
          LowestIDInHBQ = werkzeug:minNrSL(State#status.hbq),
          HighestIDinDLQ = maxNrSLHelper(State#status.dlq, State),

          Temp3State = case HighestIDinDLQ+1 < LowestIDInHBQ  of %wenn Luecke (von min 1 elem)
            true -> %Der naechste Eintrag in der Holdbackqueue ist groesser, als der letzte in der Deliveryqueue; Also haben wir eine Luecke die gefuellt werden muss!
			  log("Server","Luecke vorhanden von ~p bis ~p", [HighestIDinDLQ+1,LowestIDInHBQ-1]),
              MessageAndTimestamp ={string:join(["****Fehlernachricht (Lueckenfueller) fuer Nachricht", werkzeug:to_String(HighestIDinDLQ+1),"bis", werkzeug:to_String(LowestIDInHBQ-1),"um ", werkzeug:timeMilliSecond()]," "),HighestIDinDLQ+1,LowestIDInHBQ-1},

              TempState =State#status{dlq = werkzeug:pushSL(State#status.dlq, {HighestIDinDLQ+1,MessageAndTimestamp})}, %Speicher PlainText Nachricht + Lueckenstart + Lueckenende Werte in der Deliveryqueue
              TempState;


            false -> %Der naechste Eintrag in der Holdbackqueue hat die MessageID, die in der Deliveryqueue erwarted wird (keine Lücke erkannt)
              {TempID,TempMessage} = werkzeug:findSL(State#status.hbq, LowestIDInHBQ),%Kopier naechste Nachricht aus der Holdbackqueue in Variable
              log("Server","Kopiere ID ~p von der Holdbackqueue in die Deleiveryqueue", [TempID]),
			  MessageAndTimestamp = string:join([TempMessage,"|To DLQ:",werkzeug:timeMilliSecond()]," "),

              TempState = State#status{dlq = werkzeug:pushSL(State#status.dlq, {LowestIDInHBQ,MessageAndTimestamp})}, %Speicher die Nachricht in der Deliveryqueue
              Temp2State = TempState#status{hbq = werkzeug:popSL(TempState#status.hbq)}, %Entferne die (jetzt alte) Nachricht aus der Holdbackqueue popSL=kleinste nummer
              Temp2State

          end,


          %io:format("State#status.dlq: ~p~n", [Temp3State#status.dlq]),
          %io:format("State#status.hbq: ~p~n", [Temp3State#status.hbq]),
          moveMessagesFromHbqToDlq(Temp3State,Config); %Recusiver Aufruf, bis Deliveryqueue maximal gefuellt (oder keine Nachrichten mehr in der Holdbackqueue zum verschieben vorhanden)


      _ -> %Die Holdbackqueue hat (z.Zt.) keine Nachrichten mehr.
        State
      end;
    _ -> %In der Deliveryeue sind alle Plaetze belegt: d.h. nichts kopieren.
      State
  end,
  NewState.

  
deleteOldClients(State,Config)->
	_={Megaseconds,Seconds,Microseconds} = erlang:now(),
	Threshold ={Megaseconds,Seconds-Config#config.clientlifetime,Microseconds},


	XDrop =case length(State#status.clients) >0 of
		true ->
			lists:dropwhile(
					fun({_,{_,CurrentTimeStamp}}) -> 
					CurrentTimeStamp >= Threshold 

					end, State#status.clients);
		false ->
			[]
	end,
    TempStatus= State#status{clients = lists:subtract(State#status.clients,XDrop)},
	case length(XDrop) >0 of
	true ->
	log("Server","Kick alte Cliente: ~p", [TempStatus#status.clients]);
	false->
	ok
	end,
TempStatus.

updateClientRequestTime(State,ClientPID)->
	CurrentClientTupel=werkzeug:findSL(State#status.clients,ClientPID),
	 %log("Server","Update ~p", [CurrentClientTupel]),
	 {_,{CurrentValue,_}} = CurrentClientTupel,
	 
	NewState= State#status{clients = lists:delete(CurrentClientTupel,State#status.clients)},
	
	UpdatedClientTupel = {ClientPID,{CurrentValue,erlang:now()}},
	
	TempStatus= NewState#status{clients = werkzeug:pushSL(NewState#status.clients,UpdatedClientTupel)},
	TempStatus.
				
		
				
%% #############################################HELPER##################################################################

%% Setzt einen neuen Wert für den Lese Client
changeClientNextMessageIdTo(NewValue,ClientPID,State)->
  {ClientPID,{CurrentValue,CurrentTimeStamp}} = werkzeug:findSL(State#status.clients,ClientPID),

  NewState =State#status{clients = lists:delete({ClientPID,{CurrentValue,CurrentTimeStamp}},State#status.clients)}, %Altes Value loeschen
  Return = NewState#status{clients = werkzeug:pushSL(NewState#status.clients,{ClientPID,{NewValue,CurrentTimeStamp}})}, %Return State mit neuem Value für Client
  %%%%%%%%%%%io:format("[changeClientNextMessageIdTo() return]~p~n",[Return]),
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
  
  
  %%Helper###################################################################################################################
  
log(File, Message, Data) ->
  werkzeug:logging(fileDesc(File), io_lib:format("[~s] (~p) "++Message++"~n",[werkzeug:timeMilliSecond(),self()]++Data)).

fileDesc(File) ->
  ID = File++string:sub_string(werkzeug:to_String(self()),2,string:len(werkzeug:to_String(self()))-1),
  ID++".log".

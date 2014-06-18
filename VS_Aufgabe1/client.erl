-module(client).

-record(status, {
  ids =[]
}).


-record(config, {
  clients,
  lifetime,
  servername,
  sendeintervall
}).


-export([start/1]).

load_config() ->
  {ok, ConfigFile} = file:consult('client.cfg'),
  #config{
    clients = proplists:get_value(clients, ConfigFile),
    lifetime = proplists:get_value(lifetime, ConfigFile),
    servername = proplists:get_value(servername, ConfigFile),
    sendeintervall = proplists:get_value(sendeintervall, ConfigFile)
  }.

start(ServerNode) ->
  net_adm:ping(ServerNode),
  Config = load_config(),
  Status = #status{},
  io:format("~n~n[Status:] ~p~n",[Status]),
  io:format("[Config:] ~p~n~n~n",[Config]),
  Servername = Config#config.servername,
  ServerPID = global:whereis_name(Servername),
  log("Client","###Servername ~s has Pid: ~p", [Servername, ServerPID]),


  Clients = lists:map(fun(_ClientID) ->
							ClientPID = spawn_link(fun() -> redakteur(ServerPID,1,Status,Config) end),
							log("Client","Client Startzeit: ~p mit PID ~p",[werkzeug:timeMilliSecond(), ClientPID]),
							timer:exit_after(timer:seconds(Config#config.lifetime), ClientPID, "Ende Gelaende")
						end, lists:seq(1, Config#config.clients)), 
  Clients.

%%REDAKTEUR!######################################################
redakteur(ServerPid,MaxNumbers,Status,Config)->
  io:format("~n~n"),
  MessageID = getNum(ServerPid),%Nummer organisieren
  
  UpdatedStatus = Status#status{ids = Status#status.ids ++ [MessageID]},
  %io:format("~n~n~n~n+++++++UpdatedStatus ~p~n~n~n~n",[UpdatedStatus]),
  io:format("Warte fuer ~p sekunden...~n",[Config#config.sendeintervall]),timer:sleep(timer:seconds(Config#config.sendeintervall)),
  {NewStatus,NewConfig} = send(ServerPid,MessageID,MaxNumbers,UpdatedStatus,Config),%Nahricht mit der Nummer schicken
  redakteur(ServerPid,MaxNumbers+1,NewStatus,NewConfig). %Endlosschleife starten





getNum(Server) ->
  io:format("Freie Nummer abfragen~n"),
  Server ! { query_msgid, self()},
  receive
    { msgid, Number} ->
      log("Client","Nummer ~w zugeteilt durch Server", [Number]),
      Number %Returnvalue
  end.

send(ServerPid,MessageID,MaxNumbers,Status,Config) ->
  %Drop one out of six
  case MaxNumbers rem 5 of
    0 ->
      %Vergessen
      log("Client","NO MESSAGE SEND DUE TO RESTRICTION BY MISSION!",[]),
      %Lese Client Starten
      leser(ServerPid,Status),
      %Wartezeit neu Setzten
      {Status,setNewWaitingTime(Config)};

    _ ->
      %Senden
      EigenenNamen="bla",
      RechnerName=net_adm:localhost(),
      ProzessNummer=io_lib:format("~p", [ServerPid]),
      PraktikumsGruppe="02",
      TeamNummer="03",
      AktuelleSystemzeit=werkzeug:timeMilliSecond(),


      Nachricht=lists:flatten(lists:concat([EigenenNamen,"@",RechnerName,ProzessNummer,PraktikumsGruppe,TeamNummer,AktuelleSystemzeit])),
      log("Client","SENDE: ~p ~p", [MessageID, Nachricht]),


      ServerPid ! {new_message, {Nachricht, MessageID}},
      io:format(ok),
      {Status,Config} %Unveränderte Config

end.

%% Helper
setNewWaitingTime(Config)->
  Intervall = Config#config.sendeintervall,
  Vorzeichen = case random:uniform() > 0.5 of
             true -> 1;
             false -> -1
           end,
  NewInterval = round(Intervall + (Intervall/2 * Vorzeichen)),

  NewConfig = Config#config{sendeintervall = NewInterval},

  case NewConfig#config.sendeintervall < 2 of
    true -> setNewWaitingTime(NewConfig);
    false ->
	log("Client","Neue Wartezeit gesetzt auf: ~p Sekunden~n",[NewInterval]),
	NewConfig
  end.



%%LESER!################################################################################################################
leser(ServerPid,Status) ->
  log("Client","LESER GESTARTET ~p~n", [ServerPid]),

  getNext(ServerPid,Status).


getNext(ServerPid,Status) ->
  io:format("Server PID ~p | My PID ~p ~n", [ServerPid, self()]),

  ServerPid ! { query_messages, self()},
  receive

    { message, Number,Nachricht,true} -> %%Keine weiteren Nachrichten auf Server
      case lists:member(Number,Status#status.ids) of
        true ->
          log("Client"," ~w : ~s : ~w *****************~n", [Number,Nachricht,true]);
        false ->
          log("Client"," ~w : ~s : ~w n", [Number,Nachricht,true])
      end;
    { message, Number,Nachricht,false} -> %% NOCH weitere NAchrichten auf Server
      case lists:member(Number,Status#status.ids) of
			true ->
				log("Client"," ~w : ~s : ~w *****************~n", [Number,Nachricht,true]);
			false ->
				log("Client"," ~w : ~s : ~w n", [Number,Nachricht,true])
		  end,
      getNext(ServerPid,Status)
  end.

  
%%Helper###################################################################################################################
  
log(File, Message, Data) ->
  werkzeug:logging(fileDesc(File), io_lib:format("[~s] (~p) "++Message++"~n",[werkzeug:timeMilliSecond(),self()]++Data)).

fileDesc(File) ->
  ID = File++string:sub_string(werkzeug:to_String(self()),2,string:len(werkzeug:to_String(self()))-1),
  ID++".log".



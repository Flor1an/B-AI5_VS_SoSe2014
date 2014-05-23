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

-record(status, {
  ids =werkzeug:emptySL(),
  config
}).


-record(config, {
  clients,
  lifetime,
  servername,
  sendeintervall
}).

%% API
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
  Servername = Config#config.servername,
  ServerPID = {Servername,ServerNode},
  log("Client","Servername ~s has Pid: ~p", [Servername, ServerPID]),
  Lifetime = Config#config.lifetime,
  %io:format("Lifetime: ~p~n",[Lifetime]),
  NumClients = Config#config.clients,
  %io:format("NumClients: ~p~n",[NumClients]),
  Clients = lists:map(fun(_ClientID) ->
							ClientPID = spawn(fun() -> redakteur(ServerPID, Config,1) end),
							log("Client","Client Startzeit: ~p mit PID ~p",[werkzeug:timeMilliSecond(), ClientPID]),
							timer:exit_after(timer:seconds(Lifetime), ClientPID, "Ende Gelaende")
						end, lists:seq(1, NumClients)), %TODO:Testing
  Clients.

%%REDAKTEUR!######################################################
redakteur(ServerPid,Config,MaxNumbers)->
  io:format("~n~n"),
  Number = getNum(ServerPid),%Nummer organisieren
  io:format("Warte fuer ~p sekunden...~n",[Config#config.sendeintervall]),
  timer:sleep(timer:seconds(Config#config.sendeintervall)),
  NewConfig = send(ServerPid,Number,MaxNumbers,Config),%Nahricht mit der Nummer schicken
  redakteur(ServerPid,NewConfig,MaxNumbers+1). %Endlosschleife starten





getNum(Server) ->
  io:format("Freie Nummer abfragen~n"),
  Server ! { query_msgid, self()},
  receive
    { msgid, Number} ->
      log("Client","Nummer ~w zugeteilt durch Server", [Number]),
      Number %Returnvalue
  end.

send(ServerPid,Number,MaxNumbers,Config) ->
  %Drop one out of six
  case MaxNumbers rem 5 of
    0 ->
      %Vergessen
      log("Client","NO MESSAGE SEND DUE TO RESTRICTION BY MISSION!",[]),
      %Lese Client Starten
      leser(ServerPid),
      %Wartezeit neu Setzten
      setNewWaitingTime(Config);

    _ ->
      %Senden
      EigenenNamen="bla",
      RechnerName=net_adm:localhost(),
      ProzessNummer=io_lib:format("~p", [ServerPid]),
      PraktikumsGruppe="02",
      TeamNummer="03",
      AktuelleSystemzeit=werkzeug:timeMilliSecond(),


      Nachricht=string:join([EigenenNamen,"@",RechnerName,ProzessNummer,PraktikumsGruppe,TeamNummer,AktuelleSystemzeit],"-"),
      log("Client","SENDE: ~p ~s", [Number, Nachricht]),


      ServerPid ! {new_message, {Nachricht, Number}},
      io:format(ok),
      Config %Unveränderte Config

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
leser(ServerPid) ->
  log("Client","LESER GESTARTET ~p~n", [ServerPid]),

  getNext(ServerPid).


getNext(ServerPid) ->
  io:format("Server PID ~p | My PID ~p ~n", [ServerPid, self()]),

  ServerPid ! { query_messages, self()},
  receive

    { message, Number,Nachricht,true} -> %%Keine weiteren Nachrichten auf Server
      log("Client"," ~w : ~s : ~w", [Number,Nachricht,true]);

    { message, Number,Nachricht,false} -> %% NOCH weitere NAchrichten auf Server
      log("Client"," ~w : ~s : ~w", [Number,Nachricht,false]),
      getNext(ServerPid)
  end.

log(File, Message, Data) ->
  werkzeug:logging(fileDesc(File), io_lib:format("[~s] (~p) "++Message++"~n",[werkzeug:timeMilliSecond(),self()]++Data)).

fileDesc(File) ->
  ID = File++string:sub_string(werkzeug:to_String(self()),2,string:len(werkzeug:to_String(self()))-1),
  ID++".log".



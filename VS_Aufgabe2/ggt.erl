-module(ggt).
-export([start/8,initial/1]).
-record(config,
    {	prozessNR,
		teamID,
    	praktikumsgruppe,
		koordinatorName,
		ttw,
		ttt,
		starterNR,
		nameservicePID,
		ggtName,
		linkerName,
		rechterName,
		letzterEmpfangEinerZahl,
		tttTimer,
		erfGemeldetTerms}).
-include("messages.hrl").
-include("constants.hrl").

	
start(KoordinatorName, TTW, TTT, ProzessNR, StarterNR, NameservicePID, Praktikumsgruppe, TeamID) ->
	GGTName = list_to_atom(lists:flatten(io_lib:format("~p~p~p_~p", [ Praktikumsgruppe, TeamID, ProzessNR, StarterNR ]))),
	Config = #config{koordinatorName=KoordinatorName,
					ttw=TTW,
					ttt=TTT,
					prozessNR=ProzessNR,
					starterNR=StarterNR,
					nameservicePID=NameservicePID, 
					praktikumsgruppe=Praktikumsgruppe, 
					teamID=TeamID,
					ggtName=GGTName,
					erfGemeldetTerms=0
					},

	logH(Config,"SETUP"),	
	spawn(ggt, initial, [Config]).


% 2. Zustand initial:
initial(Config)->
	logH(Config,"INITIAL"),
	
	%2.1.3 Er registriert sich ebenfalls lokal auf der Erlang-Node mit seinem Namen (register).
	register(Config#config.ggtName,self()),
	
	%2.1.2 Der ggT-Prozess registriert sich beim Namensdienst (rebind).
	Config#config.nameservicePID ! {self(), {?REBIND, Config#config.ggtName, node()}},
	log(Config,"REBIND"),

	%2.1.1 Der ggT-Prozess meldet sich beim Koordinator mit seinem Namen an (check_in)....
	lookup(Config,Config#config.koordinatorName) ! {check_in, Config#config.ggtName},
	log(Config,"CHECK_IN"),
	
	
	%2.2.1 Der ggT-Prozess erwartet dann die Informationen über seine Nachbarn (setneighbours). 
	receive 
		% {set_neighbours,LeftN,RightN}: die (lokal auf deren Node registrierten und im Namensdienst registrierten) 
		% Namen des linken und rechten Nachbarn werden gesetzt. LeftN und RightN sind die Namen der Nachbarn.
		{set_neighbours, Left, Right} -> 
			log(Config,"Nachbarn wurden mitgeteilt {L: ~p | R: ~p}",[Left,Right]),
			NewConfig=Config#config{linkerName=Left, rechterName=Right},
			%2.2.2 Wechsel in den Zustand pre_process
			WorkerPID = spawn_link(fun() -> pre_process(NewConfig) end),
			dispatcherLoop(Config,WorkerPID,initial,0)
	end.

%Syncroner Loop fuer asyncronische Aufrufe in meinem Worker 
dispatcherLoop(Config,WorkerPID,AktuellerStatus,AktuellerMi) ->

	receive 
		% Vom KOORDINATOR ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		% Sendet das aktuelle Mi an From: From ! {mi,Mi}. Wird vom Koordinator z.B. genutzt, um bei einem Berechnungsstillstand die Mi-Situation im Ring anzuzeigen.
		{tell_mi, From} -> 
			log(Config,"~p erfragt aktuellen Mi. Sende Mi = ~p",[From,AktuellerMi]),
			From ! {mi,AktuellerMi},
			dispatcherLoop(Config,WorkerPID,AktuellerStatus,AktuellerMi);
			
		% Sendet ein {i_am, State} an den Koordinator, in dem der aktuelle Zustand mitgeteilt wird. 
		% Wird vom Koordinator z.B. genutzt, um auf manuelle Anforderung hin die Lebendigkeit des Rings zu prüfen.
		{whats_on,From} ->
			log(Config, "~p erfragt aktuellen Status. Sende ~p",[From,AktuellerStatus]),
			lookup(Config,Config#config.koordinatorName) ! {i_am, AktuellerStatus},
			dispatcherLoop(Config,WorkerPID,AktuellerStatus,AktuellerMi);
			
		% kill der ggT-Prozess wird beendet.
		kill -> 
			Config#config.nameservicePID ! {self(),{?UNBIND, Config#config.ggtName}},
			logH(Config,"KILLED :("),
			exit(WorkerPID,"KILL empfangen"), %Worker killen
			exit("KILL empfangen"); % Dispatcher killen
		
		%Vom WORKER ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		{update_mi, Mi} ->
			dispatcherLoop(Config,WorkerPID,AktuellerStatus,Mi);
			
		{update_state, State} ->
			dispatcherLoop(Config,WorkerPID,State,AktuellerMi);
		
		% Rest an den eigentlichen Worker weiterleiten
		Other -> 
			log(Config,"DISPATCHER: Leite Befehl ~p an meinen Worker weiter...",[Other]),
			WorkerPID ! Other,
			dispatcherLoop(Config,WorkerPID,AktuellerStatus,AktuellerMi)
	end.
	
	

% AUFGABE 3
pre_process(Config) ->
	logH(Config,"PRE_PROCESS"),
	lookup(Config,Config#config.ggtName) ! {update_state, pre_process}, %Dispatcher Informieren
	%3.1.1 Zustand pre_process: Der ggT-Prozess erwartet vom Koordinator seine Zahl Mi (set_pmi).
	receive 
		% {set_pmi,MiNeu}: die von diesem Prozess zu berabeitenden Zahl für eine neue Berechnung wird gesetzt.
		{set_pmi,Mi} -> 
			log(Config,"Initial Mi wurde mitgeteilt. Mi:~p",[Mi]),
			% 5.3.1 
			{ok, Timer} = timer:send_after(timer:seconds(Config#config.ttt), votehelper),
			NewConfig = Config#config{tttTimer=Timer, letzterEmpfangEinerZahl=now()},
			%3.1.2 Wechsel in den Zustand process.
			processEntryPoint(NewConfig,Mi)
	end.
	

processEntryPoint(Config,Mi)->
	logH(Config,"PROCESS"),
	lookup(Config,Config#config.ggtName) ! {update_state, process}, %Dispatcher Informieren
	process(Config,Mi).
	
% AUFGABE (4+) 5
process(Config,Mi)->

	receive 
	
		% 5.3 Der ggT-Prozess beobachtet die Zeit seit dem letzten Empfang einer Zahl (send oder set_pmi). 
		% 	  Hat diese <ttt> Sekunden überschritten, startet er eine Terminierungsanfrage / Abstimmung (vote). 
		%	  Es wird (von ihm) nur genau eine Terminierungsanfrage gestartet.
		votehelper ->
			log(Config,"TTT Timer ist abgelaufen. Informiere meine linken Nachbarn (~p) ueber vote",[Config#config.linkerName]),
			lookup(Config,Config#config.linkerName) ! {vote,Config#config.ggtName},
			process(Config,Mi);
		
		% 4. Der ggT-Prozess kann zu jeder Zeit zu einer neuen Berechnung aufgefordert werden! (copy n' paste von pre_process)
		{set_pmi,Mi} -> 
			log(Config,"Neuer Mi wurde mitgeteilt (Neue berechnung). Mi:~p",[Mi]),
			lookup(Config,Config#config.ggtName) ! {update_mi, Mi}, %Dispatcher Informieren
			timer:cancel(Config#config.tttTimer),
			{ok, Timer} = timer:send_after(timer:seconds(Config#config.ttt), votehelper),
			NewConfig = Config#config{tttTimer=Timer, letzterEmpfangEinerZahl=now()},
			process(NewConfig,Mi);

		% 5.1.1 Wenn er eine Zahl erhält (send) führt er den ggT-Algorithmus aus.
		{send,Y} ->
			log(Config,"Send erhalten Y:~p",[Y]),
			Mi2=algo(Mi, Y, Config),
			lookup(Config,Config#config.ggtName) ! {update_mi, Mi2}, %Dispatcher Informieren
			%io:format("Mi: ~p Mi2: ~p", [Mi, Mi2]),
			timer:cancel(Config#config.tttTimer),
			{ok, Timer} = timer:send_after(timer:seconds(Config#config.ttt), votehelper),
			NewConfig = Config#config{tttTimer=Timer, letzterEmpfangEinerZahl=now()},	
				
			case Mi2 =/= Mi of
				true -> 
					% 5.1.2 Verändert sich auf Grund einer Berechnung der Wert #Mi, so informiert er den Koordinator darüber (brief_mi) 
					log(Config,"informiere (brief_mi) Koordinator ~p ueber neuen Mi:~p",[ Config#config.koordinatorName, Mi2]),
					lookup(Config,Config#config.koordinatorName) ! {brief_mi, {Config#config.ggtName, Mi2, werkzeug:timeMilliSecond()}},
					% ... und seine linken und rechten Nachbarn (send).
					log(Config,"Sende (send) Nachbarn ~p und ~p neuen Mi:~p",[ Config#config.linkerName, Config#config.rechterName, Mi2]),
					lookup(Config,Config#config.linkerName) ! {send, Mi2}, 
					lookup(Config,Config#config.rechterName) ! {send, Mi2};
				_ ->
					% 5.1.3 Ändert sich seine Zahl dadurch nicht, macht der ggT-Prozess gar nichts und erwartet die nächste Nachricht.
					log(Config,"Mi NICHT geaendert! (ist und bleibt: ~p)",[Mi])		
			end,
			process(NewConfig,Mi2);
		
		
		% 5.5.1 Ein ggT-Prozess erhält die Anfrage nach der Terminierung (vote)
		{vote, Initiator} ->
			lookup(Config,Config#config.ggtName) ! {update_state, vote}, %Dispatcher Informieren
			timer:cancel(Config#config.tttTimer),
			log(Config,"### vote erhalten! Ich = ~p  Initiator = ~p) ###",[Config#config.ggtName, Initiator]),
			
			NewConfig = case Initiator == Config#config.ggtName of
				
				true -> % 5.4.1 Ist die Abstimmung erfolgreich (vote wird ihm mit seinem Namen gesendet),
					lookup(Config,Config#config.ggtName) ! {update_state, voted}, %Dispatcher Informieren
					% 5.4.2 sendet er dem Koordinator eine Mitteilung über die Terminierung (brief_term) 
					%		mit seinem Namen, dem errechneten ggT (sein aktuelles Mi) und seine aktuelle Systemzeit.
					log(Config,"Abstimmung erfolgreich! informiere (brief_term) Koordinator ~p ueber errechneten GGT:~p",[ Config#config.koordinatorName, Mi]),
					lookup(Config,Config#config.koordinatorName) ! {brief_term,{Config#config.ggtName, Mi, werkzeug:timeMilliSecond()},self()},

					BisherMeldungen = Config#config.erfGemeldetTerms,
					% 5.4.3 Zudem zählt er seine erfolgreich gemeldeten Terminierungsmeldungen und notiert dies in seinem log.
					log(Config,"Abstimmung erfolgreich! Zaehle meine 'erfolgreich gemeldeten Terminierungsmeldungen hoch' Wert jetzt: ~p",[BisherMeldungen+1]),
					Config#config{erfGemeldetTerms=BisherMeldungen+1};
				
				_ -> % 5.5.1 ...und er ist nicht der Initiator.
					% 5.5.1.2 ist seit dem letzten Empfang einer Zahl ...
					TimeDiff=timer:now_diff(now(), Config#config.letzterEmpfangEinerZahl),
					% ... mehr als <ttt>/2 Sekunden vergangen ...
					case (TimeDiff > timer:seconds((Config#config.ttt/2))) of
						true ->
							% ... dann leitet er die Anfrage an seinen linken Nachbarn weiter (implizites Zustimmen). 
							log(Config, "Leite Anfrage (vote) an den Linken Nachbarn ~p weiter (implizites Zustimmen)",[Config#config.linkerName]),
							lookup(Config,Config#config.linkerName) ! {vote, Initiator};
						_ -> 
							% Sonst ignoriert er die Nachricht (implizites ablehnen).
							log(Config, "implizit abgelehnt")
					end,
					Config
					
			end,
			
			process(NewConfig,Mi)
	end.
 
algo(Mi, Y, Config) -> 
	log(Config,"ALGO: ((~p -1) mod ~p) +1     --    (TTW: ~p Sekunden)",[Mi,Y,Config#config.ttw]),
	% 5.2.1 Für eine ggT-Berechnung braucht er jedoch eine gewisse Zeit (<ttw>). Dies simuliert eine größere, 
	%		Zeit intensivere Aufgabe. Der ggT-Prozess soll in dieser Zeit einfach nichts tun (timer:sleep).
	timer:sleep(timer:seconds(Config#config.ttw)),
 	NeuerMi = case Y < Mi of
				true -> 
					((Mi-1) rem Y)+1;
				_ -> 
					Mi
				end,
	NeuerMi.
			


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


lookup(Config, Name) ->
	Config#config.nameservicePID ! {self(), {?LOOKUP, Name}},
	receive
		{?LOOKUP_RES, ServiceAtNode} ->
		ServiceAtNode
	end.
  
%%%%%%%%%%%%%
	
logH(Config,Message)->
	GgtName = lists:concat(["GGT(", Config#config.ggtName,")@", net_adm:localhost()]),
	LogMessage = lists:concat([io_lib:nl(),io_lib:nl(),
							GgtName,
							" ",
                            werkzeug:timeMilliSecond(),
                            " ######################## ",
							Config#config.ggtName,
							" Status: ",
                            Message,
							" ########################",
                            io_lib:nl()]),
	werkzeug:logging(lists:concat([GgtName,".log"]), LogMessage).	
	
%Default Log
log(Config,Message)->
	GgtName = lists:concat(["GGT(", Config#config.ggtName,")@", net_adm:localhost()]),
	LogMessage = lists:concat([GgtName,
							" ",
                             werkzeug:timeMilliSecond(),
                             " ",
                             Message,
                             io_lib:nl()]),
	werkzeug:logging(lists:concat([GgtName,".log"]), LogMessage).

log(Config,Message,Prams)->
	Flat = io_lib:format(Message, Prams),
	log(Config,Flat).  
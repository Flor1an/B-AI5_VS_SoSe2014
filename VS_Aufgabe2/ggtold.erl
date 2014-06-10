-module(ggtold).
-export([start/8, init/8]).
-record(config,
    {	teamID=14,
    	praktikumsgruppe=02,
		koordPID,
		ttw,
		ttt,
		prozessNR,
		starterNR,
		namensDPID,
		myID,
		leftN,
		rightN,
		logfile,
		lastSendy}).


nice_format(Format, Args) ->
	lists:flatten(io_lib:format("~s ~n",[lists:flatten(io_lib:format(Format, Args))])).	

gen_myid(PG, TID, PNR, SNR) ->
	io:format("gen_myid ++++++++++\n\nPG: ~p~nTID: ~p~nPNR: ~p~nSNR: ~p~n---~n",[PG, TID, PNR, SNR]),
	lists:flatten(io_lib:format("~p~p~p_~p", [ PG, TID, PNR, SNR ])).


	
start(KoordPID,TTW, TTT,ProzessNR, StarterNR,NamensDPID, Praktikumsgruppe, TeamID) ->
	io:format("~n~n~n~n==============~p============~n~n~n~n~n", [NamensDPID]),
	%init(KoordPID,TTW, TTT,ProzessNR, StarterNR,NamensDPID, Praktikumsgruppe, TeamID).
	spawn(ggtold, init, [KoordPID,TTW, TTT,ProzessNR, StarterNR,NamensDPID, Praktikumsgruppe, TeamID]).
	

% AUFGABE 2
init(KoordPID,TTW, TTT,ProzessNR, StarterNR,NamensDPID, Praktikumsgruppe, TeamID)->
	io:format("blablabla~n",[]),
	io:format("Init(~p)~n",[{KoordPID,TTW, TTT,ProzessNR, StarterNR,NamensDPID, Praktikumsgruppe, TeamID}]),
	Config = #config{koordPID=KoordPID, ttw=TTW, ttt=TTT, prozessNR=ProzessNR, starterNR=StarterNR, namensDPID=NamensDPID, praktikumsgruppe=Praktikumsgruppe, teamID=TeamID},
	io:format("**1:Config:~p~n", [Config]),
	
	NewConfig2 = Config#config{myID=list_to_atom(gen_myid(Config#config.praktikumsgruppe, Config#config.teamID, Config#config.prozessNR, Config#config.starterNR))},
	io:format("**2:NewConfig2:~p~n", [NewConfig2]),
	
	NewConfig = NewConfig2#config{logfile="GGTP_" ++ atom_to_list(NewConfig2#config.myID) ++ "@" ++ net_adm:localhost() ++ ".log"},
	io:format("**3:NewConfig:~p~n", [NewConfig]),
	
	%2.1.3 Er registriert sich ebenfalls lokal auf der Erlang-Node mit seinem Namen (register).
	register(NewConfig#config.myID,self()),
	

	%2.1.2 Der ggT-Prozess registriert sich beim Namensdienst (rebind).
	io:format("%% BEFORE SEND ~p%%~n",[{Config#config.namensDPID ,{self(), {rebind, NewConfig#config.myID, node()}}}]),
	Config#config.namensDPID ! {self(), {rebind, NewConfig#config.myID, node()}},
	io:format("%% AFTER SEND %%~n",[]),
	receive 
		ok -> werkzeug:logging(NewConfig#config.logfile, nice_format("~p bindet to: ~p", [util:timestamp(),Config#config.namensDPID]));
		kill	-> killingstuff(Config)
	after 5000000 -> werkzeug:logging(NewConfig#config.logfile, nice_format("~p no respomnse from nameservice: ~p", [util:timestamp(),Config#config.namensDPID]))
	end,

	NewerConfig = NewConfig, 
	%2.1.1 Der ggT-Prozess meldet sich beim Koordinator mit seinem Namen an (check_in)....
	werkzeug:logging(NewConfig#config.logfile, nice_format("~p send hello to koord (~p) with myID (~p)", [util:timestamp(), NewerConfig#config.koordPID, NewConfig#config.myID])),
	NewerConfig#config.koordPID ! {hello, NewConfig#config.myID},
	
	%2.2.1 Der ggT-Prozess erwartet dann die Informationen über seine Nachbarn (setneighbours). 
	receive 
		{setneighbors, LeftN, RightN} -> 
			werkzeug:logging(NewConfig#config.logfile, nice_format("~p got Neighbours ~p and ~p", [util:timestamp(),LeftN, RightN])),
			EvenNewerConfig=NewerConfig#config{leftN=LeftN, rightN=RightN};
		kill	->EvenNewerConfig=Config, killingstuff(Config)
	
	after 600000000 ->	werkzeug:logging(NewConfig#config.logfile, nice_format("~p got no answer from the boss of it all", [util:timestamp()])), EvenNewerConfig = NewerConfig
	end,

	NewestConfig = resolve_neighbors(EvenNewerConfig),
	
	%2.2.2 Wechsel in den Zustand pre_process
	pre_process(NewestConfig).

% AUFGABE 3
pre_process(Config) ->
	%3.1.1 Zustand pre_process: Der ggT-Prozess erwartet vom Koordinator seine Zahl Mi (set_pmi).
	receive 
		{setpm,MiNeu} -> 
			Mi=MiNeu,
			werkzeug:logging(Config#config.logfile, nice_format("~p got new Mi: ~p", [util:timestamp(),MiNeu])),
			% 5.3.1 
			NewConfig=Config#config{lastSendy=now()};
		kill ->
			NewConfig=Config, Mi=0, killingstuff(Config)
	end,
	%3.1.2 Wechsel in den Zustand process.
	process(NewConfig,Mi).

% AUFGABE (4) 5
process(Config,Mi)->
	Temp=timer:now_diff(now(), Config#config.lastSendy),
	if 
		Temp > Config#config.ttt*1000 -> 
			werkzeug:logging(Config#config.logfile, nice_format("~p DIfferenz: ~p initiate abstimmung",[util:timestamp(),Temp])), 
			Config#config.rightN !{abstimmung,Config#config.myID},
			abstimmung(Config,Mi);
		true -> foo
	end,
	receive 

		% 5.1.1 Wenn er eine Zahl erhält (send) führt er den ggT-Algorithmus aus.
		{sendy,Y}  ->
				werkzeug:logging(Config#config.logfile, nice_format("~p received sendy : ~p",[util:timestamp(),Y])),
				Mi2=algo(Mi, Y, Config#config.ttw,Config),
				io:format("Mi: ~p Mi2: ~p", [Mi, Mi2]),
				% 5.3.1 
				NewConfig=Config#config{lastSendy=now()},
				
				
				case Mi2 =/= Mi of
					true -> 
						% 5.1.2 Ändert sich seine Zahl dadurch, informiert er zusätzlich den Koordinator (Nachricht brief_mi).
						sendToNs(NewConfig,Mi2),
						NewConfig#config.koordPID ! {briefmi, {NewConfig#config.myID, Mi2, werkzeug:timeMilliSecond()}},
						werkzeug:logging(Config#config.logfile, nice_format("~p Mi (~p) did change to ~p",[util:timestamp(), Mi,Mi2])),
						process(NewConfig, Mi2);
					_ ->
						% 5.1.3 Ändert sich seine Zahl dadurch nicht, macht der ggT-Prozess gar nichts und erwartet die nächste Nachricht.
						werkzeug:logging(Config#config.logfile, nice_format("~p Mi didn't change. Mi: ~p Mi2: ~p",[util:timestamp(),Mi,Mi2])), 
						process(NewConfig, Mi)
				end;
				
		% 5.4.1 Ist die Abstimmung erfolgreich (vote wird ihm mit seinem Namen gesendet),
		{abstimmung, From} when From == Config#config.myID ->
			% 5.4.2 sendet er dem Koordinator eine Mitteilung über die Terminierung (brief_term) mit seinem Namen, 
			%		dem errechneten ggT (sein aktuelles Mi) und seine aktuelle Systemzeit.
			Config#config.koordPID ! {briefterm,{Config#config.myID,Mi,werkzeug:timeMilliSecond()}}, 
			% 5.4.3 Zudem zählt er seine erfolgreich gemeldeten Terminierungsmeldungen und notiert dies in seinem log.
			werkzeug:logging(Config#config.logfile, nice_format("~p send end of algorithm (briefterm) to koordinator. Mi: ~p",[util:timestamp(),Mi])),
			abstimmung(Config, Mi);

			
		% 5.3.2 Hat diese <ttt> Sekunden überschritten, startet er eine Terminierungsanfrage / Abstimmung (vote).
		{abstimmung, From} ->
			TimeDiff=timer:now_diff(now(), Config#config.lastSendy),
			if (TimeDiff > (Config#config.ttt/2)*1000) -> 
					werkzeug:logging(Config#config.logfile, nice_format("~p forward abstimmung. Mi: ~p",[util:timestamp(),Mi])),
					% 5.3.3 Es wird (von ihm) nur genau eine Terminierungsanfrage gestartet.
					Config#config.rightN!{abstimmung,From},
					process(Config,Mi);%,abstimmung(Config,Mi);
			 	true -> 
					werkzeug:logging(Config#config.logfile, nice_format("~p ignore abstimmung; not finished yet. Mi: ~p",[util:timestamp(),Mi])),
					process(Config,Mi)
			end;
					 
					 

		% Sendet das aktuelle "Mi" an "From"
		{tellmi, From} -> 
			From ! Mi, process(Config, Mi);
					
		kill -> 
			Config#config.namensDPID ! {self(),{unbind, Config#config.myID}}, killingstuff(Config);
			{setpm,MiNeu} ->
				werkzeug:logging(Config#config.logfile, nice_format("~p got new Mi: ~p", [util:timestamp(),MiNeu])),
				MostNewConfig=Config#config{lastSendy=now()},
				process(MostNewConfig,MiNeu)

	% 5.2.1 Für eine ggT-Berechnung braucht er jedoch eine gewisse Zeit (<ttw>). Dies simuliert eine größere, 
	%		Zeit intensivere Aufgabe. Der ggT-Prozess soll in dieser Zeit einfach nichts tun (timer:sleep). 
	%		Während einer echten Berechnung ist der <ttt> Timer zu unterbrechen. 
	%		Nach der Berechnung werden der <ttt> Timer und das letzte Empfangsereignis aktualisiert.
	after 
		Config#config.ttt 	-> 
			werkzeug:logging(Config#config.logfile, nice_format("~p initiate abstimmung (after block). Mi: ~p",[util:timestamp(),Mi])), 
			Config#config.rightN ! {abstimmung, Config#config.myID}, 
			abstimmung(Config, Mi)
	
	end
	.
 
algo(Mi, Y, TTW,Config) ->  % 21.
			werkzeug:logging(Config#config.logfile,nice_format("~p algo(Mi: ~p, Y: ~p, TTW: ~p)",[util:timestamp(),Mi, Y, TTW])),
			timer:sleep(TTW),
 	 		case Y < Mi of
				true -> 
					io:format("TEMP TEMP: (~p -1 mod ~p) +1 ~n ",[Mi,Y]),
					((Mi-1) rem Y)+1;
				_ -> 
					Mi
			end.
			
sendToNs(Config, Mi)	-> werkzeug:logging(Config#config.logfile, nice_format("~p send sendy (~p) to ~p and ~p",[util:timestamp(), Mi, Config#config.leftN, Config#config.rightN])), Config#config.leftN ! {sendy, Mi}, Config#config.rightN ! {sendy, Mi}.

	
	
resolve_neighbors(Config) ->
	% rechten nachbarnamen aufl�sen
	Config#config.namensDPID ! {self(),{lookup, Config#config.leftN}},
	LeftName = Config#config.leftN,
	receive
		not_found -> werkzeug:logging(Config#config.logfile, nice_format("~p Linker Nachbar ~p konnte nicht aufgelost werden.", [util:timestamp(),Config#config.leftN])), LeftNTupel = {};
		LeftNTupel = {LeftName, Node1} -> werkzeug:logging(Config#config.logfile, nice_format("~p Linker Nachbar ~p befindet sich auf node ~p.", [util:timestamp(),LeftName, Node1]))
	end,

	% linken nachbarnamen aufl�sen
	Config#config.namensDPID ! {self(),{lookup, Config#config.rightN}},
	RightName = Config#config.rightN,
	receive
		not_found -> werkzeug:logging(Config#config.logfile, nice_format("~p Rechter Nachbar ~p konnte nicht aufgelost werden.", [util:timestamp(),Config#config.rightN])), RightNTupel = {};
		RightNTupel = {RightName, Node2} -> werkzeug:logging(Config#config.logfile, nice_format("~p Rechter Nachbar ~p befindet sich auf node ~p.", [util:timestamp(),RightName, Node2]))
	end,
	Config#config{leftN = LeftNTupel, rightN = RightNTupel}.

% 5.5.1.0 Die Abstimmung arbeitet wie folgt
abstimmung(Config, Mi)	->  
	receive 
		kill	-> 
			Config#config.namensDPID ! {self(),{unbind, Config#config.myID}}, 
			killingstuff(Config);
		
		%TODO RECHTER NACHBAR !!: 5.5.2.1 Erhält ein initiierender Prozess von seinem rechten Nachbarn die Anfrage nach der Terminierung (vote), 
		{abstimmung, MYID} when Config#config.myID==MYID ->
			werkzeug:logging(Config#config.logfile, nice_format("~p send end of algorithm (briefterm) to koordinator. Mi: ~p",[util:timestamp(),Mi])),
			% 5.5.2.2 meldet er die Terminierung dem Koordinator
			Config#config.koordPID ! {briefterm,{Config#config.myID,Mi,werkzeug:timeMilliSecond()}}, 
			abstimmung(Config, Mi);
			
		% 5.5.1.1 Ein ggT-Prozess erhält die Anfrage nach der Terminierung (vote) und er ist nicht der Initiator:
		{abstimmung, OtherID} ->  
			werkzeug:logging(Config#config.logfile, nice_format("~p (~p ) forward abstimmung. Mi: ~p",[util:timestamp(),OtherID,Mi])),
			%TODO: 5.5.1.2 Zeit **/2 implementieren und an den LINKEN! schicken
			Config#config.rightN ! {abstimmung, OtherID}, 
			abstimmung(Config, Mi);

		% Aufruf der ggT Berechnung + zustands�nderung in "aktiv berechnen", zur�ck zu process(...)
		{sendy,Y}  ->
				werkzeug:logging(Config#config.logfile, nice_format("~p received sendy : ~p",[util:timestamp(),Y])),
				Mi2=algo(Mi, Y, Config#config.ttw,Config),
				io:format("Mi: ~p Mi2: ~p", [Mi, Mi2]),
				NewConfig=Config#config{lastSendy=now()},
				case Mi2 =/= Mi of
					true -> sendToNs(NewConfig,Mi2),
						NewConfig#config.koordPID ! {briefmi, {NewConfig#config.myID, Mi2, werkzeug:timeMilliSecond()}},
						werkzeug:logging(Config#config.logfile, nice_format("~p Mi (~p) did change to ~p",[util:timestamp(), Mi,Mi2])),
						process(NewConfig, Mi2);
					_ ->
						werkzeug:logging(Config#config.logfile, nice_format("~p Mi didn't change. Mi: ~p Mi2: ~p",[util:timestamp(),Mi,Mi2])), process(NewConfig, Mi)
				end;
		{tellmi, From} -> From ! Mi, abstimmung(Config, Mi);
		{setpm,MiNeu} ->
			werkzeug:logging(Config#config.logfile, nice_format("~p got new Mi: ~p", [util:timestamp(),MiNeu])),
			MostNewConfig=Config#config{lastSendy=now()},
			process(MostNewConfig,MiNeu)
	end.
		
killingstuff(Config) -> 
	Config#config.namensDPID! {self(),{unbind, Config#config.myID}},
	werkzeug:logging(Config#config.logfile, nice_format("~p ~p is DEAD!", [util:timestamp(),Config#config.myID])).

	
%Default Log
log(Number,Message)->
	StarterName= lists:concat(["GGT(", Number,")@", net_adm:localhost()]),
	LogMessage = lists:concat([StarterName,
							" ",
                             werkzeug:timeMilliSecond(),
                             " ",
                             Message,
                             io_lib:nl()]),
	werkzeug:logging(lists:concat([StarterName,".log"]), LogMessage).

log(Number,Message,Prams)->
	Flat = io_lib:format(Message, Prams),
	log(Number,Flat).  
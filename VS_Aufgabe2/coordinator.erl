-module(coordinator).

-export([start/1]).

-record(state, { clients=orddict:new(),
				smallesMi,
				toggle
               }).
-record(gcd_client, { name
                    , left_neighbor
                    , right_neighbor
                    }).
-record(config, {
				nameservicename,
				rt,
				pts,
				ttw,
				ttt,
				myname
}).
-include("messages.hrl").
-include("constants.hrl").

% PUBLIC ##############################################
start(NameserviceNode) ->
	setup(NameserviceNode).
	
% /PUBLIC ##############################################
  
load_config() ->
	{ok, ConfigFile} = file:consult('coordinator.cfg'),
	#config{
			nameservicename = proplists:get_value(nameservicename, ConfigFile),
			rt 		= proplists:get_value(rt, ConfigFile),	%RegisterTime
			pts 		= proplists:get_value(pts, ConfigFile), %ProcessesToStart
			ttw 		= proplists:get_value(ttw, ConfigFile), %TimeToWork
			ttt 		= proplists:get_value(ttt, ConfigFile), %TimeToTerminate
			myname 		= proplists:get_value(myname, ConfigFile)
	}.
	
	
setup(NameserviceNode) ->
	io:format("~n~n~n~n~n~n~n"),
	logH("SETUP"),
	State = #state{smallesMi=null,toggle=false},
	Config = load_config(),
	register(Config#config.myname, self()),
	global:register_name(Config#config.myname, self()),
	log("NameServiceNode = ~p", [NameserviceNode]),
	
	net_adm:ping(NameserviceNode),
	log("NameServiceName = ~p", [Config#config.nameservicename]),
	
	NameServicePID = global:whereis_name(Config#config.nameservicename),
	log("NameServicePID  = ~p", [NameServicePID]),	
	case NameServicePID == undefined of
		true ->
			timer:sleep(500),
			io:format("~n~n~n"),
			exit("NameServicePID ist undefined. Bitte nochmal versuchen.");
		_ ->
			now()
	end,
	

	% IP 2 Starten des Koordinators mit Registrierung beim Namensdienst registriert
	NameServicePID ! {self(), {?REBIND, Config#config.myname, node()}},
	receive
		{?REBIND_RES, ok} ->
			log("Mit NameService verbunden"),		
			registerEntryPoint(State,Config)
	end.

% 3.0 Nach den <rt> Sekunden geht der Koordinator in den Zustand init über. Er gibt keinem Starter mehr Auskunft und registriert keine ggT-Prozesse mehr!
registerEntryPoint(State,Config) ->
	logH("REGISTER"),
	log("Warte ~p sekunden lang auf Registrierungen",[Config#config.rt]),
	timer:send_after(timer:seconds(Config#config.rt),self(), moveToInit),
	registerState(State,Config).
  
% 2.0
registerState(State,Config) ->
	io:format("~n~n"),
	
	receive
		% 2.0 Nach dem Start des Koordinators können sich Starter und/oder ggT-Prozesse innerhalb der <rt>Sekunden bei ihm melden.
		{check_in, NewGGTName} -> % 1x pro GGT Node
			UpdatedClients = orddict:store(NewGGTName, #gcd_client{name=NewGGTName}, State#state.clients),
			NewState = State#state{clients=UpdatedClients},
			log("Neuer GGT Prozess mit dem Name: ~p hat sich angemeldet (check_in)", [NewGGTName]),
			registerState(NewState,Config); %Loop für weitere Anfragen

		
		
		% 2.1 In diesem Zustand (register) können Startern die benötigten Informationen über 
		%	  Anzahl der zu startenden ggT-Prozesse, deren jeweilige Verzögerungszeit <ttw> und deren Terminierungszeit <ttt> erfragen.
		{get_ggt_vals, From} -> % 1x pro STARTER
		
			log("Starter ~p erfragt die GGT Startwerte (get_ggt_vals)", [From]),
			From ! {ggt_vals,
						Config#config.ttw,
						Config#config.ttt,
						Config#config.pts
						},
			log("... sende ggt_vals (TTW: ~p TTT: ~p PTS: ~p)", [Config#config.ttw,Config#config.ttt,Config#config.pts]),
			registerState(State,Config); %Loop für weitere Anfragen

		% Wird nach ablauf der <rt> Zeit automatisch aufgerufen
		moveToInit ->
			init(State,Config);
			
		Other ->
			logH(Other)
  end,
  
  
  ok.

% 3. Nach den <rt> Sekunden geht der Koordinator in den Zustand init über. 
%	 Er gibt keinem Starter mehr Auskunft und registriert keine ggT-Prozesse mehr!
init(State,Config) -> 
	logH("INIT"),
	log("Warte auf dem step Befehl"),
	
	receive 
		% 4.1. Über step wird der Koordinator veranlasst den ggT Ring aufzubauen. Die Reihenfolge soll zufällig bestimmt werden.
		% IP 5.1 Aufbau eines zufällig gemischten Rings durch den Koordinator (Trigger ist die Nachricht step)
		step ->			
			log("step erhalten. Erstelle den Ring"),
	
			StateWithRing =ringErstellen(State,State#state.clients),  
			
			% IP 5.2 der die ggT Prozesse über ihren linken und rechten Nachbarn informiert (set_neighbours).
			set_neighbours(StateWithRing,Config),
	
			% 4.3 Danach wechselt der Koordinator inf den Zustand ready.
			readyEntryPoint(StateWithRing,Config);
			
		kill ->
			killing(State,Config);

		reset ->
			reseting(State,Config)
	end.
	
	
	
readyEntryPoint(State,Config) ->
	logH("READY"),
	log("Warte auf steuer Befehle (z.b. calc)"),
	ready(State,Config).
  
% 5 Im Zustand ready:
ready(State,Config) ->
	receive
  
	% 5.0
    calc ->
		Target = random:uniform(100), %Beliebige Zahl zwischen 1 und 100
		log("Schluesselwort ~p empfangen (generiertes target=~p)",[calc,Target]),
		% 5.1 Der Koordinator informiert alle ggT-Prozesse über ihre Startwerte (set_pmi)
		set_pmi(State, Config, Target),
		% 5.3 Er startet die Berechnung und sendet 15% der ggT Prozesse (mindestens 2 und zufällig gewählt) eine Zahl (Vielfaches von target) über send
		send(State,Config, Target),
		% loop
		ready(State,Config);
	  
	% 5.0 (Für per Hand) Starten einer Berechnung über die Nachricht {calc target}.
	%					 target ist der gewünschte ggt (dieser ist zur manuellen Überprüfung der Berechnung gedacht).
    {calc, Target} when is_integer(Target) andalso Target > 0 ->
		log("Schluesselwort ~p empfangen mit target=~p",[calc,Target]),
		% 5.1 Der Koordinator informiert alle ggT-Prozesse über ihre Startwerte (set_pmi)
		set_pmi(State, Config, Target),
		% 5.3 Er startet die Berechnung und sendet 15% der ggT Prozesse (mindestens 2 und zufällig gewählt) eine Zahl (Vielfaches von target) über send
		send(State,Config, Target),
		% loop
		ready(State,Config);
	
	%Ein ggT-Prozess mit Namen ggtName informiert über sein neues Mi ggTMi um ggTZeit Uhr.
    {brief_mi, {GGTName, GGTMi, GGTZeit}} ->
		log("Prozess ~p hat einen neuen Mi ~p um ~p berechnet", [GGTName, GGTMi, GGTZeit]),
		ready(State,Config);
	  
	% 5.4 Der Koordinator wird von den ggT-Prozessen über deren Terminierung informiert (brief_term).
	% Ein ggT-Prozess mit Namen ggtName und PID From informiert über die Terminierung mit Ergebnis ggTMi um ggTZeit Uhr.
    {brief_term, {GGTName, GGTMi, GGTZeit}, From} -> 
	
		BissherigesMi = case State#state.smallesMi == null of 
			true ->%Noch nicht gesetzt
				GGTMi;
			_ ->
				State#state.smallesMi
		end,
		case GGTMi > BissherigesMi of
			true ->
				log("#### FEHLER !! Kleinster Mi = ~p neuer Mi von ~p = ~p ; gesendet von ~p",[BissherigesMi,GGTName,GGTMi,From]),
				%5.4.3 Ist ein spezielles Flag (Nachricht toggle) gesetzt, sendet er dem ggT-Prozess die kleinste Zahl per send.
				case State#state.toggle == true of
					true ->
						NameServicePID = global:whereis_name(Config#config.nameservicename),
						lookup(NameServicePID,GGTName) ! {send, BissherigesMi};
					false ->
						void
				end,
				ready(State,Config);
			_ ->
				log("GGT wurde berechnent: Mi >> ~p << berechnet durch: ~p um: ~p", [GGTMi, GGTName, GGTZeit]),
				NewState = State#state{smallesMi = GGTMi},
				ready(NewState,Config)
		end;	
	
	% Der Koordinator erfragt bei allen ggT-Prozessen per tell_mi deren aktuelles Mi ab und zeigt es im log an.
	prompt ->
		log("prompt empfangen; Liste die aktuelle Mi's aller GGT Prozesse:"),
		ClientsNamesList = orddict:fetch_keys(State#state.clients),
		NameServicePID = global:whereis_name(Config#config.nameservicename),
		lists:map(
			fun(ClientName) ->
				lookup(NameServicePID,ClientName) ! {tell_mi,self()},
				receive
					{mi,AktuellerMi} ->
						log("  ~p = ~p",[ClientName,AktuellerMi])
				end				
			end,
			ClientsNamesList),
		ready(State,Config);
	
	% Der Koordinator erfragt bei allen ggT-Prozessen per whats_on deren Lebenszustand ab und zeigt dies im log an.
	whats_on ->
		log("whats_on empfangen; Liste die aktuelle Status aller GGT Prozesse:"),
		ClientsNamesList = orddict:fetch_keys(State#state.clients),
		NameServicePID = global:whereis_name(Config#config.nameservicename),
		lists:map(
			fun(ClientName) ->
				lookup(NameServicePID,ClientName) ! {whats_on,self()},
				receive
					{i_am,AktuellerStatus} ->
						log("  ~p = ~p",[ClientName,AktuellerStatus])
				end
			end,
			ClientsNamesList),
		ready(State,Config);
		
	% 5.4.3 Ist ein spezielles Flag (Nachricht toggle) gesetzt, sendet er dem ggT-Prozess die kleinste Zahl per send.
	% Der Koordinator verändert den Flag zur Korrektur bei falschen Terminierungsmeldungen.
	toggle ->
		NewState=toggle(State),
		ready(NewState,Config);

	% 5.5.1 Per manueller Eingabe kann der Koordinator in den Zustand "beenden" (Nachricht kill)
	% Der Koordinator wird beendet und sendet allen ggT-Prozessen das kill-Kommando.
	kill ->
		killing(State,Config);

	% 5.5.2 ... oder in den Zustand register (Nachricht reset) versetzt werden.
	% Der Koordinator sendet allen ggT-Prozessen das kill-Kommando und bringt sich selbst in den Zustand, indem sich Starter wieder melden können.
    reset ->
		reseting(State,Config)

	end.
	


	
toggle(State) ->
		NewState= State#state{toggle=true},
		NewState.
		%sendet er dem ggT-Prozess die kleinste Zahl per send


reseting(State,Config) ->
	logH("RESET"),
	ggtsKillen(Config,State#state.clients),
	% 5.5.3 Beim Übergang in den Zustand register wird die Konfigurationsdatei des Koordinators erneut gelesen.
	FreshConfig = load_config(),
	FreshState = State#state{clients=orddict:new(), smallesMi=null},
	registerEntryPoint(FreshState,FreshConfig).

killing(State,Config) ->
	logH("KILL"),
	ggtsKillen(Config,State#state.clients),
	log("Unbind vom Namensdienst"),
	NameServicePID = global:whereis_name(Config#config.nameservicename),
    NameServicePID ! {self(), {?UNBIND, Config#config.myname}},
	exit("kill erhalten :(").

ggtsKillen(Config,GGTProzesse) ->
	ClientsNamesList = orddict:fetch_keys(GGTProzesse),
	NameServicePID = global:whereis_name(Config#config.nameservicename),
	lists:map(
			fun(ClientName) ->
				log("Leite ~p an ~p weiter", [kill,ClientName]),
				lookup(NameServicePID,ClientName) ! kill
			end,
			ClientsNamesList).
	
ringErstellen(State, GGTs) ->
	log("Erstelle Ring:"),
	GgtProcs = orddict:fetch_keys(GGTs),
	log("## Sortierte GGTs: ~p",[GgtProcs]),
	ShuffledGgtProcs = werkzeug:shuffle(GgtProcs),
	log("## Gemixte GGTs: ~p",[ShuffledGgtProcs]),
	ringErstellen(State,ShuffledGgtProcs, length(ShuffledGgtProcs)).
	
ringErstellen(State,_, 0) -> State;
ringErstellen(State, GgtProcs, Index) ->
	PreviousIndex = case (Index-1) =< 0 of
					true ->
						length(GgtProcs); %Letzter
					_ ->
						(Index-1)
					end,
					
					
	NextIndex	  = case (Index+1) > length(GgtProcs) of
					true ->
						1; % Erster
					_ ->
						(Index+1)
					end,
	Left = lists:nth(PreviousIndex, GgtProcs),
	Current = lists:nth(Index, GgtProcs),
	Right = lists:nth(NextIndex, GgtProcs),
	log("GGT ~p = linker Nachbar: ~p | rechter Nachbar: ~p",[Current,Left,Right]),
	
	UpdatedClients = orddict:store(Current, #gcd_client{left_neighbor=Left, right_neighbor=Right}, State#state.clients),
	NewState=State#state{clients=UpdatedClients},
	
	%Current ! {?NEIGHBOURS, Left, Right},
	ringErstellen(NewState, GgtProcs, Index - 1).

	
	

% IP 5.2 der die ggT Prozesse über ihren linken und rechten Nachbarn informiert (set_neighbours).
set_neighbours(State,Config) ->
	log("GGT Prozesse ueber Nachbarn informieren"),
	NameServicePID = global:whereis_name(Config#config.nameservicename),
	orddict:map(
		fun(Key, Value) ->
			log("Sende ~p seine Nachbarn: R=~p | L=~p",[Key,Value#gcd_client.left_neighbor,Value#gcd_client.right_neighbor]),
			lookup(NameServicePID,Key) ! {set_neighbours,
                                       Value#gcd_client.left_neighbor,
                                       Value#gcd_client.right_neighbor}
		end,
    State#state.clients).

% 5.1 Der Koordinator informiert alle ggT-Prozesse über ihre Startwerte (set_pmi)
set_pmi(State,Config, Target) ->
	NameServicePID = global:whereis_name(Config#config.nameservicename),
		
	orddict:map(
		fun(Key, _) ->
			VielfachesVonTarget = lists:nth(1,werkzeug:bestimme_mis(Target,1)),
			log("Informiere ~p: ueber initial Mi = ~p", [Key, VielfachesVonTarget]),
			lookup(NameServicePID,Key) ! {set_pmi, VielfachesVonTarget}
		end,
    State#state.clients).

% 5.3 Er startet die Berechnung und sendet 15% der ggT Prozesse (mindestens 2 und zufällig gewählt) eine Zahl (Vielfaches von target) über send
send(State,Config, Target) ->
	Clients = State#state.clients,
	ClientsNamesList = orddict:fetch_keys(Clients),
	%%% select 15% of the clients but at least 2 clients
	SelectedClientNames = get15Percent(ClientsNamesList),
	log("15% der GGT Prozess ausgewaehlt: ~p",[SelectedClientNames]),
	NameServicePID = global:whereis_name(Config#config.nameservicename),
	
	lists:map(
		fun(GGTProzess) ->
			VielfachesVonTarget = lists:nth(1,werkzeug:bestimme_mis(Target,1)),
			log("Informiere ~p mit dem Vielfachen von Target = ~p", [GGTProzess, VielfachesVonTarget]),
			lookup(NameServicePID,GGTProzess) ! {send, VielfachesVonTarget}
		end,
		SelectedClientNames),
	ok.


get15Percent(List) ->
	RemainingElementsToSelect = case round((length(List)/100) * 15) < 2 of
		true -> 
			2;

		_ -> 
			round((length(List)/100) * 15)
	end,
	
	get15Elemtnts(werkzeug:shuffle(List), RemainingElementsToSelect, []).

get15Elemtnts(_List, 0, Accu) ->
	Accu;
get15Elemtnts(List, RemainingElementsToSelect, Accu) ->
	[Head | Tail] = List,
	get15Elemtnts(Tail, RemainingElementsToSelect - 1, [Head | Accu]).


	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup(NameservicePID, Name) ->
	NameservicePID ! {self(), {?LOOKUP, Name}},
	receive
		{?LOOKUP_RES, ServiceAtNode} ->
			ServiceAtNode
	end.
	
%%%%%%%%%%%%%

%Log for Heading
logH(Message)->
	CoordinatorName= lists:concat(["Coordinator@", net_adm:localhost()]),
	LogMessage = lists:concat([io_lib:nl(),io_lib:nl(),
							CoordinatorName,
							" ",
                            werkzeug:timeMilliSecond(),
                            " ######################## Status: ",
                            Message,
							" ########################################",
                            io_lib:nl()]),
	werkzeug:logging(lists:concat([CoordinatorName,".log"]), LogMessage).
%Default Log
log(Message)->
	CoordinatorName= lists:concat(["Coordinator@", net_adm:localhost()]),
	LogMessage = lists:concat([CoordinatorName,
							" ",
                             werkzeug:timeMilliSecond(),
                             " ",
                             Message,
                             io_lib:nl()]),
	werkzeug:logging(lists:concat([CoordinatorName,".log"]), LogMessage).

log(Message,Prams)->
	Flat = io_lib:format(Message, Prams),
	log(Flat).  
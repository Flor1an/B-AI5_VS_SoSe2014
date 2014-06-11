-module(coordinator).

-export([start/1,calc/0,calc/1,reset/0,kill/0]).

-record(state, { clients=orddict:new(),
				smallesMi
               }).
-record(gcd_client, { name
                    , servicepid
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
	
	
calc() -> %5.1
	sentToMyself(calc).

calc(GCD) when is_integer(GCD) andalso GCD > 0 -> % 5.0
	sentToMyself({calc, GCD}).

reset() ->
	sentToMyself(reset).

kill() ->
	sentToMyself(kill).

% /PUBLIC ##############################################
  
load_config() ->
	{ok, ConfigFile} = file:consult('coordinator.cfg'),
	#config{
			nameservicename = proplists:get_value(nameservicename, ConfigFile),
			rt 				= proplists:get_value(rt, ConfigFile),	%RegisterTime
			pts 			= proplists:get_value(pts, ConfigFile), %ProcessesToStart
			ttw 			= proplists:get_value(ttw, ConfigFile), %TimeToWork
			ttt 			= proplists:get_value(ttt, ConfigFile), %TimeToTerminate
			myname 			= proplists:get_value(myname, ConfigFile)
	}.
	
	
setup(NameserviceNode) ->
	io:format("~n~n~n~n~n~n~n"),
	logH("SETUP"),
	State = #state{smallesMi=[10000000000000000000000000000]},
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
	log("Warte auf dem step befehl"),
	
	receive 
		% 4.1. Über step wird der Koordinator veranlasst den ggT Ring aufzubauen. Die Reihenfolge soll zufällig bestimmt werden.
		% IP 5.1 Aufbau eines zufällig gemischten Rings durch den Koordinator (Trigger ist die Nachricht step)
		step ->			
			log("step erhalten. Erstelle den Ring"),
	
			ClientsWithRing = step_ring_bauen(State#state.clients),
			StateWithRing = State#state{clients = ClientsWithRing},

			% IP 5.2 der die ggT Prozesse über ihren linken und rechten Nachbarn informiert (set_neighbours).
			set_neighbours(StateWithRing),
	
			% 4.3 Danach wechselt der Koordinator inf den Zustand ready.
			readyEntryPoint(StateWithRing,Config)
	end.

readyEntryPoint(State,Config) ->
	logH("READY"),
	ready(State,Config).
  
% 5 Im Zustand ready:
ready(State,Config) ->
	receive
  
	% 5.0
    calc ->
		Target = random:uniform(100), %Beliebige Zahl zwischen 1 und 100
		% 5.1 Der Koordinator informiert alle ggT-Prozesse über ihre Startwerte (set_pmi)
		set_pmi(State, Target),
		% 5.3 Er startet die Berechnung und sendet 15% der ggT Prozesse (mindestens 2 und zufällig gewählt) eine Zahl (Vielfaches von target) über send
		send(State, Target),
		% loop
		ready(State,Config);
	  
	% 5.0 (Für per Hand) Starten einer Berechnung über die Nachricht {calc target}.
	%					 target ist der gewünschte ggt (dieser ist zur manuellen Überprüfung der Berechnung gedacht).
    {calc, Target} when is_integer(Target) andalso Target > 0 ->
		% 5.1 Der Koordinator informiert alle ggT-Prozesse über ihre Startwerte (set_pmi)
		set_pmi(State, Target),
		% 5.3 Er startet die Berechnung und sendet 15% der ggT Prozesse (mindestens 2 und zufällig gewählt) eine Zahl (Vielfaches von target) über send
		send(State, Target),
		% loop
		ready(State,Config);
	
	%Ein ggT-Prozess mit Namen ggtName informiert über sein neues Mi ggTMi um ggTZeit Uhr.
    {briefmi, {GGTName, GGTMi, GGTZeit}} ->
		log("Prozess ~p hat einen neuen Mi ~p um ~p berechnet", [GGTName, GGTMi, GGTZeit]),
		ready(State,Config);
	  
	% 5.4 Der Koordinator wird von den ggT-Prozessen über deren Terminierung informiert (brief_term).
	% Ein ggT-Prozess mit Namen ggtName und PID From informiert über die Terminierung mit Ergebnis ggTMi um ggTZeit Uhr.
    {brief_term, {GGTName, GGTMi, GGTZeit}, From} -> % TODO: noch viel flo
		BissherigesMi = State#state.smallesMi,
		case GGTMi > BissherigesMi of
			true ->
				log("°°°°°°°°°° FEHLER !! Kleinster Mi = ~p neuer Mi von ~p = ~p",[BissherigesMi,GGTName,GGTMi]),
				ready(State,Config);
			_ ->
				log("Client ~p finished calculation with Mi ~p at ~p", [GGTName, GGTMi, GGTZeit]),
				NewState = State#state{smallesMi = GGTMi},
				ready(NewState,Config)
		end;	
	
	% Der Koordinator erfragt bei allen ggT-Prozessen per tell_mi deren aktuelles Mi ab und zeigt es im log an.
	%prompt ->
		%TODO
	
	% Der Koordinator erfragt bei allen ggT-Prozessen per whats_on deren Lebenszustand ab und zeigt dies im log an.
	%whats_on ->
		%TODO
		
	% 5.4.3 Ist ein spezielles Flag (Nachricht toggle) gesetzt, sendet er dem ggT-Prozess die kleinste Zahl per send.
	% Der Koordinator verändert den Flag zur Korrektur bei falschen Terminierungsmeldungen.
	toggle ->
		toggle(State,Config);

	% 5.5.1 Per manueller Eingabe kann der Koordinator in den Zustand "beenden" (Nachricht kill)
	% Der Koordinator wird beendet und sendet allen ggT-Prozessen das kill-Kommando.
	kill ->
		killing(State,Config);

	% 5.5.2 ... oder in den Zustand register (Nachricht reset) versetzt werden.
	% Der Koordinator sendet allen ggT-Prozessen das kill-Kommando und bringt sich selbst in den Zustand, indem sich Starter wieder melden können.
    reset ->
		reseting(State)

	end.
	
toggle(State,Config) ->
		io:format("TODO~p~p~n",[State,Config]).
	%sendet er dem ggT-Prozess die kleinste Zahl per send


reseting(State) ->
	logH("RESET"),
	kill_all_gcd_clients(State),
	% 5.5.3 Beim Übergang in den Zustand register wird die Konfigurationsdatei des Koordinators erneut gelesen.
	FreshConfig = load_config(),
	FreshState = State#state{clients=orddict:new(), smallesMi=[10000000000000000000000000000]},
	registerEntryPoint(FreshState,FreshConfig).


killing(State,Config) ->
	logH("KILL"),
  %%% send the kill command to all registered clients
  kill_all_gcd_clients(State),

  log("Trying to unbind coordinator name at nameservice"),
  case global:whereis_name(nameservice) of
    undefined -> ok; %% do nothing, if nameservice not available
    Nameservice ->
      Nameservice ! {self(), {unbind, Config#config.myname}}
  end,
  log("Terminating coordinator process. Goodbye."),
  exit(self()).



%%% update the clients dictionary with another client
update_clients_with_client(Clients, ClientName, UpdatedClient) ->
  orddict:store(ClientName, UpdatedClient, Clients).

%%% register gcd client and return new state
addGGTNode(State, Config, NewGGTNodeName) ->
	ClientList = State#state.clients,
	NameServicePID = global:whereis_name(Config#config.nameservicename),

	lookup(NameServicePID,NewGGTNodeName),
	
	NewGGTNodePID = case nameservice_lookup(NameServicePID, NewGGTNodeName) of
		not_found ->
			log_error(format("Client: ~p not found at nameservice", [NewGGTNodeName])),
			not_found;

		%%% everything is good. return servicepid.
		{ok, ServicePid} -> 
			ServicePid;

		error ->
			log_error("register gcd client: nameservice_lookup was interrupted."),
			error
	end,

	UpdatedClients = orddict:store(NewGGTNodeName, #gcd_client{name=NewGGTNodeName, servicepid=NewGGTNodePID}, ClientList),
	State#state{clients=UpdatedClients}.

%%% build a ring of the registered gcd clients where each gcd client
%%% knows his left and right neighbor.
%%% Pivot: first ClientName from which we start building the ring
%%% Clients: State#state.clients Dictionary with ClientName -> gcd_client entries
%%%
%%% Returns: an updated Clients Dictionary
step_ring_bauen(Clients) ->
  log("Build ring of gcd clients"),
  %%% it is not possible/ill-adviced to build a ring with only one client.
  %%% that client would have himself as his left and right neighbor and would
  %%% send himself 2 messages.
  %%% TODO decide if we should increment this to < 3 to have distinct neighbors
  case orddict:size(Clients) < 2 of
    true ->
		log_error("Building the GCD ring failed."),
		log_error("Building a ring of less than two clients is not possible"),
		error;
    false ->
		ClientsList = werkzeug:shuffle(orddict:fetch_keys(Clients)),
		[Pivot | Tail] = ClientsList,
		step_ring_bauen(Clients,
                                orddict:fetch(Pivot, Clients),
                                Tail,
                                none)
  end.

step_ring_bauen(Clients, Pivot, RemainingClientsList, none) ->
	%%% initial call:
	step_ring_bauen(Clients,
                            Pivot,
                            RemainingClientsList,
                            Pivot);

step_ring_bauen(Clients, Pivot, [], PreviousClient) ->
  %%% empty RemainingClientsList:
  %%% all clients have been updated. all that is missing is the left neighbor
  %%% of the Pivot element and the right_neighbor of the PreviousClient
  {ok, PivotFromClientsDictionary} = orddict:find(Pivot#gcd_client.name, Clients),
  FinishedPivot = PivotFromClientsDictionary#gcd_client{left_neighbor=PreviousClient#gcd_client.name},

  %%% 3. set FinishedPivot as the right_neighbor of the PreviousClient
  FinishedPreviousClient = PreviousClient#gcd_client{right_neighbor=FinishedPivot#gcd_client.name},

  %%% return the updated Clients Dictionary with the ring
  UpdatedClients = update_clients_with_client(Clients,
                                              FinishedPivot#gcd_client.name,
                                              FinishedPivot),

  log("Building the ring of gcd clients succeeded"),
  update_clients_with_client(UpdatedClients,
                             FinishedPreviousClient#gcd_client.name,
                             FinishedPreviousClient);

step_ring_bauen(Clients, Pivot, RemainingClientsList, PreviousClient) ->
  %%% there are remaining clients. recursively traverse the RemainingClientsList:
  %%% 1. get the CurrentClient from the Clients Dictionary
  %%% we dont match for error because if this fails we have a problem anyway
  %%% and want the process to throw an exception for now
  [Head | Tail] = RemainingClientsList,
  {ok, CurrentClient} = orddict:find(Head, Clients),

  %%% 1. set the PreviousClient as the left_neighbor of the current client
  %%% 2. set the head of the RemainingClientsList as the right_neighbor
  %%% of the current client
  UpdatedClient = CurrentClient#gcd_client{left_neighbor=PreviousClient#gcd_client.name},

  %%% 3. set UpdatedClient as the right_neighbor of the PreviousClient
  FinishedPreviousClient = PreviousClient#gcd_client{right_neighbor=UpdatedClient#gcd_client.name},

  %%% 4. update the Client Dictionary with the UpdatedClient and
  %%% FinishedPreviousClient
  UpdatedClients = update_clients_with_client(Clients,
                                              UpdatedClient#gcd_client.name,
                                              UpdatedClient),
  UpdatedClients2 = update_clients_with_client(UpdatedClients,
                                               FinishedPreviousClient#gcd_client.name,
                                               FinishedPreviousClient),
  %%% 3. set the RemainingClientsList to the tail of RemainingClientsList
  step_ring_bauen(UpdatedClients2,
                            Pivot,
                            Tail,
                            UpdatedClient).

% IP 5.2 der die ggT Prozesse über ihren linken und rechten Nachbarn informiert (set_neighbours).
set_neighbours(State) ->
  log("Introducing GCD clients to their neighbors"),
  orddict:map(
    fun(Key, Value) ->
        log(format("set GCD client ~p: left neighbor: ~p, right neighbor: ~p",  [Key,Value#gcd_client.left_neighbor,Value#gcd_client.right_neighbor])),
        Value#gcd_client.servicepid ! {setneighbours,
                                       Value#gcd_client.left_neighbor,
                                       Value#gcd_client.right_neighbor}
    end,
    State#state.clients).

% 5.1 Der Koordinator informiert alle ggT-Prozesse über ihre Startwerte (set_pmi)
set_pmi(State, Target) ->
	orddict:map(
		fun(Key, Value) ->
			VielfachesVonTarget = werkzeug:bestimme_mis(Target,1),
			log(format("The GCD process ~p: initial Mi ~p", [Key, VielfachesVonTarget])),
			Value#gcd_client.servicepid ! {set_pmi, VielfachesVonTarget}
		end,
    State#state.clients).

% 5.3 Er startet die Berechnung und sendet 15% der ggT Prozesse (mindestens 2 und zufällig gewählt) eine Zahl (Vielfaches von target) über send
send(State, Target) ->
	Clients = State#state.clients,
	ClientsNamesList = orddict:fetch_keys(Clients),
	%%% select 15% of the clients but at least 2 clients
	SelectedClientNames = get15Percent(ClientsNamesList),
	lists:map(
		fun(ClientName) ->
			VielfachesVonTarget = werkzeug:bestimme_mis(Target,1),
			io:format("VielfachesVonTarget = ~p~n~n~n",[VielfachesVonTarget]),
			log("send_message_to_service( ~p )", [{ClientName, VielfachesVonTarget}]),
			Client = orddict:fetch(ClientName, Clients),
			Client#gcd_client.servicepid ! {send, VielfachesVonTarget}
		end,
		SelectedClientNames),
	ok.

kill_all_gcd_clients(State) ->
  Clients = State#state.clients,
  ClientsNamesList = orddict:fetch_keys(Clients),

  lists:map(
    fun(ClientName) ->
      log(format("Sending the kill command to GCD-process ~p", [ClientName])),
      Client = orddict:fetch(ClientName, Clients),
      Client#gcd_client.servicepid ! kill
    end,
    ClientsNamesList).

		

get15Percent(List) ->
	RemainingElementsToSelect = case round((length(List)/100) * 15) < 2 of
		true -> 
			2;

		_ -> 
			round((length(List)/100) * 15)
	end,

	selectPercentageOfElementsFromList(List, RemainingElementsToSelect, []).

selectPercentageOfElementsFromList(_List, 0, Accu) ->
	Accu;

selectPercentageOfElementsFromList(List, RemainingElementsToSelect, Accu) ->
	[Head | Tail] = werkzeug:shuffle(List),
	selectPercentageOfElementsFromList(Tail,
                                          RemainingElementsToSelect - 1,
                                          [Head | Accu]).






  


format(String, ArgumentsList) ->
	io_lib:format(String, ArgumentsList).

%%%
%%% Helpers for message sending
%%%
nameservice_lookup(NameService, ServiceName) ->
  log(format("Searching for service ~p at the nameservice~n~n", [ServiceName])),

  NameService ! {self(), {lookup, ServiceName}},

  receive
    not_found ->
      log_error(format("Search for service ~p at the nameservice failed.", [ServiceName])),
      not_found;
    ServiceAddress = {ServiceName, ServiceNode} when
      is_atom(ServiceName) and is_atom(ServiceNode) ->
      {ok, ServiceAddress}

    %%% we do not want to get stuck due to an unexpected message
    %Unknown ->
      %log_error(format("nameservice_lookup: waiting for nameservice response but got: ~p.", [Unknown])),
       %error
  end.

%%% ping the nameservice in order to introduce our nodes to each other
ping_name_service(NameServiceNode) ->
  case net_adm:ping(NameServiceNode) of
    pong ->
      global:sync(),
      {ok, global:whereis_name(nameservice)};

    _ ->
      log_error("Cannot find NameService"),
      error
  end.



sentToMyself(Message) ->
	io:format("°°°°°°°°°°°°°°°°°°°sentToMyself(~p)~n",[Message]),
	Config = load_config(),
	
	%%% ping NameServiceNode
	case ping_name_service(Config#config.nameservicename) of
		{ok, NameService} ->
			%%% lookup the name and node of the current coordinator in charge
			case nameservice_lookup(NameService, Config#config.myname) of
				not_found ->
					log_error(format("Service: ~p not found at nameservice", [Config#config.myname])),
					not_found;

				%%% everything is good. send message.
				{ok, ServicePid} -> 
					ServicePid ! Message;

				error ->
					log_error("send_message_to_service: nameservice_lookup was interrupted. No message sent."),
					sentToMyself(Message)
			end;

		_ ->
			log_error("NameService was not found in send_message_to_service function"),
			error
	end.
	
	
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
                            " °°°°°°°°°°°°°°°°°°°°°°°° Status: ",
                            Message,
							" °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°",
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
  
log_error(ErrorMessage) ->
	Message = lists:concat(["##### ","Error: ", ErrorMessage, " #####"]),
	log(Message).
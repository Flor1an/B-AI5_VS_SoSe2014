
-module(starter).


-record(config, {praktikumsgruppe,teamnummer,nameservicenode,nameservicename,koordinatorname}).

-import(util, [timestamp/0]).
-import(werkzeug,[get_config_value/2]).
-include("messages.hrl").
-include("constants.hrl").

-compile([export_all]).

load_config() ->
	{ok, ConfigFile} = file:consult('ggt.cfg'),
	#config{
			praktikumsgruppe = proplists:get_value(praktikumsgruppe, ConfigFile),
			teamnummer 		 = proplists:get_value(teamnummer, ConfigFile),
			nameservicenode  = proplists:get_value(nameservicenode, ConfigFile), 
			nameservicename  = proplists:get_value(nameservicename, ConfigFile), 
			koordinatorname  = proplists:get_value(koordinatorname, ConfigFile)
	}.

% 4. Beim Starten des Starters wird die Starternummer mitgegeben.
start(Nummer) ->
	% 2. Der Starter liest aus der Datei ggt.cfg Werte
	Config = load_config(),

	net_adm:ping(Config#config.nameservicenode),
	log(Nummer,"NameServiceName = ~p", [Config#config.nameservicenode]),
	
	NameServicePID = global:whereis_name(Config#config.nameservicename),
	log(Nummer,"NameServicePID = ~p", [NameServicePID]),	
	
	case NameServicePID == undefined of
		true ->
			timer:sleep(500),
			io:format("~n~n~n"),
			exit("NameServicePID ist undefined. Bitte nochmal versuchen.");
		_ ->
			now()
	end,
	
	Cord = lookup(NameServicePID, Config#config.koordinatorname),
	io:format("###lookup returns: ~p~n",[Cord]),

	% 1.0 Der Starter (mit eindeutiger Nummer) erfragt beim Koordinator die steuernden Werte (get_ggt_vals) asynchron
	Cord ! {get_ggt_vals, self()},

	receive
		% 1.1 ...und erwartet einen entsprechenden Rückruf (ggt_vals).
		{ggt_vals, TTW, TTT, PTS} ->
			log(Nummer,"Erstelle GGT-Prozess (TimeToWork: ~p , TimeToTerminate: ~p , ProcessesToStart: ~p)",[ TTW, TTT, PTS]),
			% 3. Der Starter startet die ggT-Prozesse mit den zugehörigen Werten:
			LauncheGGTNodes = fun(ProzessNr) ->
										ggt:start(
											Config#config.koordinatorname,
											TTW,
											TTT,
											ProzessNr,
											Nummer,
											NameServicePID,	
											Config#config.praktikumsgruppe,
											Config#config.teamnummer),
										log(Nummer,"GGT-Prozess Nr: ~p wurde gestartet",[ProzessNr]),
										timer:sleep(timer:seconds(1))
								  
							  end,
			lists:foreach(LauncheGGTNodes, lists:seq(1, PTS))
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup(NameservicePID, Name) ->
	NameservicePID ! {self(), {?LOOKUP, Name}},
	receive
		{?LOOKUP_RES, ServiceAtNode} ->
			ServiceAtNode
	end.
	
%%%%%%%%%%%%%


%Default Log
log(Number,Message)->
	StarterName= lists:concat(["Starter(", Number,")@", net_adm:localhost()]),
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


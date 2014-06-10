
-module(starter).


-record(config, {praktikumsgruppe,teamnummer,nameservicenode,koordinatorname}).

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
			koordinatorname  = proplists:get_value(koordinatorname, ConfigFile)
	}.

% 4. Beim Starten des Starters wird die Starternummer mitgegeben.
start(Nummer) ->
	% 2. Der Starter liest aus der Datei ggt.cfg die folgenden Werte
	Config = load_config(),
	net_adm:ping(Config#config.nameservicenode),
	global:sync(),
	Nameservice = global:whereis_name(Config#config.nameservicenode),
	Nameservice ! {self(), {lookup, Config#config.koordinatorname}},

	receive
		{KoordinatorName, KoordinatorNode} ->
			log(Nummer,"Koordinator ~p in der Node ~p gebunden",[KoordinatorName,KoordinatorNode]),
			% 1.0 Der Starter (mit eindeutiger Nummer) erfragt beim Koordinator die steuernden Werte (get_ggt_vals) asynchron
			{KoordinatorName, KoordinatorNode} ! {get_ggt_vals, self()},

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
											Nameservice,	
											Config#config.praktikumsgruppe,
											Config#config.teamnummer),
										log(Nummer,"GGT-Prozess Nr: ~p wurde gestartet",[ProzessNr]),
										timer:sleep(timer:seconds(1))
								  
						  end,
					lists:foreach(LauncheGGTNodes, lists:seq(1, PTS))
			end
	end.

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


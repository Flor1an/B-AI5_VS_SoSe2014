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
  hbq =orddict:new(),
  dq = orddict:new(),
  clients=orddict:new()
}).


% Server starten und unter Namen "theserver" global registrieren
% Alternative: Servername aus config Datei lesen
start() ->
% {ok, ConfigListe} = file:consult("server.cfg"),
% {ok, Lifetime} = werkzeug:get_config_value(lifetime, ConfigListe),
% {ok, HostName} = inet:gethostname(),
% Datei = lists:concat(["NServer@",HostName,".log"]),


  State = #status{current_msg_id = 0},

  Pid = spawn(myserver,server,[State]),
  global:register_name(theserver,Pid).



% Verarbeiten der Daten und Antwort Ergebnis
% von fn(...) an Client senden
server(Status) ->
  receive

    {query_messages, From} -> %Abfragen aller Nachrichten (Nacheinander)

      %TempStatus= Status#status{clients = orddict:store(From,1,Status#status.clients)},
      io:format("[Clients]: ~p~n",[Status#status.clients]),
      Return = query_messages(Status,From),

      {NewStatus,{MsgId,Nachricht,Terminated}} = Return, %Aufsplitten

      From ! {message, MsgId,Nachricht,Terminated},% SIGNATURE: message,Number,Nachricht,Terminated
      server(NewStatus);

    {new_message, {Nachricht,Number}} -> %Senden einer Nachricht
      Return = new_message(Status,{Nachricht,Number}),
      %NO REPLY REQUIRED
      server(Return);

    {query_msgid, From} -> %Request current number for message
      Return = query_msgid(Status),
      Number = Return#status.current_msg_id,
      From ! {msgid,Number}, %SIGNATURE:  msgid, Number
      io:format("Returning ~w~n", [Number]),
      server(Return)

  end.

query_messages(State,Client) ->
  io:format("~n~n[query_messages METHODE]~n~n"),

  NewState = moveMessagesFromHbqToDq(State),
  io:format("~n~n[back in query_messages METHODE]~n~n"),
  io:format("[DQ] ~p~n",[orddict:size(NewState#status.dq)]),
  io:format("[HBQ] ~p~n",[orddict:size(NewState#status.hbq)]),

  case orddict:find(Client, NewState#status.clients) of

    {ok,_} -> %Client in Liste vorhanden
      io:format("[CLIENT VORHANDEN (getting next id)] ~n"),
      {_,IDToTransfer} = orddict:find(Client, NewState#status.clients),
      io:format("[FakeID] ~p ~n",[IDToTransfer]),
      io:format("[DQ] ~p ~n",[NewState#status.dq]),



      {TempStatus,MessageToTransfer} = case orddict:find(IDToTransfer,NewState#status.dq) of
                            {ok, _} -> %ID vorhanden
                              T0 = orddict:fetch(IDToTransfer,NewState#status.dq), %Get Message
                              {T1,T2} = case T0 of
                                {Message,F,T} -> %Gap Message
                                  T22 = Message,
                                  T11 =NewState#status{clients = orddict:update_counter(Client,T-F+1,NewState#status.clients)}, % increment counter with gap space
                                  {T11,T22};
                                Message -> %Normal Message
                                  T22 = Message,
                                  T11 =NewState#status{clients = orddict:update_counter(Client,1,NewState#status.clients)}, % increment counter
                                  {T11,T22}
                              end,

                              {T1,T2};

                            _ -> %nicht vorhanden also dummy sendne
                              T2 = "DUMMY MESSAGE (keine neuen NAchrichten vorhanden)",
                              T1 = NewState,
                              {T1,T2}
                          end,
      io:format("[getHighestIDinDQ((TempStatus#status.dq))] ~p~n",[getHighestIDinDQ(TempStatus)]),
      BoolToTransfer = case IDToTransfer >= getHighestIDinDQ(TempStatus)  of
                         true -> true;
                         false -> false
                       end,
      io:format("[thru... hochzählen] ~n"),


      io:format("[Clients]: ~p~n",[TempStatus#status.clients]),
      %Return
      {TempStatus,{IDToTransfer,MessageToTransfer,BoolToTransfer}};

    _ -> %Nicht vorhanden? Initial anlegen
      io:format("[CLIENT NOCH NCIHT VORHANDEN (wird angelegt)] ~n"),
      %io:format("[dq] ~p~n",[NewState#status.dq]),
      TempStatus= NewState#status{clients = orddict:store(Client,1,NewState#status.clients)},
      io:format("[ClientList]: ~p~n",[TempStatus#status.clients]),
      query_messages(TempStatus,Client)

  end.

new_message(State,{Nachricht,Number}) ->
  io:format("[new_message METHODE]~n"),
  io:format("Recived: (~w) ~s ~n",[Number,Nachricht]),
  NachrichtAndTimeStamp = string:concat(Nachricht,werkzeug:timeMilliSecond()),
  NewState = State#status{hbq =  orddict:store(Number,NachrichtAndTimeStamp,State#status.hbq)},
  %NewState = State#status{hbq = lists:append(State#status.hbq, {Nachricht})},
  io:format("[NEW LIST]~p~n",[orddict:to_list(NewState#status.hbq)]),
  NewState.



query_msgid(State) ->
  io:format("[query_msgid METHODE]~n"),
  NewState = State#status{current_msg_id = State#status.current_msg_id + 1},
  NewState.



moveMessagesFromHbqToDq(State) ->
  io:format("~n~n[moveMessagesFromHbqToDq]~n~n"),
  NewState = case orddict:size(State#status.dq) < 10 of %TODO: Use Value from Config CASE HIER: in der DQ maximal 10
    true ->
      io:format("[dq]~p~n",[orddict:size(State#status.dq)]),
      case orddict:size(State#status.hbq) >0 of
        true ->
          io:format("[hbq]~p~n",[orddict:size(State#status.hbq)]),
          LowestIDInHBQ = getLowestIDInHBQ(State),
          io:format("[KeyToMove from HBQ to DQ (if present)] ~p~n",[LowestIDInHBQ]),

          HighestIDinDQ = getHighestIDinDQ(State),
          io:format("[HighestIDinDQ] ~p~n",[HighestIDinDQ]),



          Temp3State = case LowestIDInHBQ > HighestIDinDQ+1 of %wenn lücke (min 1 elem)
            true ->
              io:format("[ID is not present in HBQ]~n"),
              MessageAndTimestamp ={string:join(["Fehlernachricht fuer Nachricht", werkzeug:to_String(HighestIDinDQ+1),"bis", werkzeug:to_String(LowestIDInHBQ-1),"um ", werkzeug:timeMilliSecond()]," "),HighestIDinDQ+1,LowestIDInHBQ-1},
              io:format("[] ~p~n",[MessageAndTimestamp]),
              TempState =State#status{dq = orddict:store(HighestIDinDQ+1,MessageAndTimestamp,State#status.dq)}, %...In die DQ
              TempState;

            false -> %also alles gut
              io:format("[ID >is< present in HBQ (all good)]~n"),
              TempMessage = orddict:fetch(LowestIDInHBQ,State#status.hbq),%Aus der HBQ...
              MessageAndTimestamp = string:concat(TempMessage,werkzeug:timeMilliSecond()),
              % io:format("~n[TempMessage]~s~n",[TempMessage]),
              TempState =State#status{dq = orddict:store(LowestIDInHBQ,MessageAndTimestamp,State#status.dq)}, %...In die DQ
              % io:format("~n[TempState]~p~n",[TempState]),
              Temp2State =TempState#status{hbq = orddict:erase(LowestIDInHBQ,TempState#status.hbq)},
              Temp2State

          end,



         % io:format("~n[Temp2State]~p~n",[Temp2State]),
         % io:format("[HBQ]~p~n",[Temp2State#status.hbq]),
         % io:format("[DQ]~p~n",[Temp2State#status.dq]),
          io:format("[Message moved]~n"),
          io:format("[NEW dq]~p~n",[orddict:size(Temp3State#status.dq)]),
          io:format("[NEW hbq]~p~n",[orddict:size(Temp3State#status.hbq)]),
          moveMessagesFromHbqToDq(Temp3State);
      _ ->
        State
      end;
    _ ->
      State
  end,

  io:format("[REturn State dq]~p~n",[orddict:size(NewState#status.dq)]),
  %NewState=State,
  NewState.



getLowestIDInHBQ(State) ->
  lists:min(orddict:fetch_keys(State#status.hbq)).

getHighestIDinDQ(State) ->
  T = case orddict:fetch_keys(State#status.dq) of
    [] ->
      1;
    _ ->
      lists:max(orddict:fetch_keys(State#status.dq))
  end,
  T.
VS_SoSe2014
===========

##Aufgabe 1

Server Terminal Starten: `erl -sname sss -setcookie bob`

Client Terminal Starten: `erl -sname ccc -setcookie bob`


Compilieren: `c(werkzeug), c(myserver), c(client).`


Server starten: `myserver:start().`

Client starten: `client:start(sss@workstation).`


##Aufgabe 2

###### Nameservice Terminal Starten `erl -sname ns -setcookie bob`

- Compilieren: `c(nameservice), c(werkzeug), c(tools), c(util).`
   
- Starten: `nameservice:start().`
 
###### Coordinator Terminal Starten `erl -sname c -setcookie bob`

- Compilieren: `c(coordinator).`
  
- Starten: `coordinator:start(ns@workstation).` *ns@workstation=Nodename*

###### Starter Terminal Starten `erl -sname s -setcookie bob`

- Compilieren: `c(starter), c(ggt).`

- Starten: `starter:start(1).` *1 = Nummer des Starters*

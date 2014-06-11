VS_SoSe2014
===========

##Aufgabe 1

###### Server Terminal Starten: `erl -sname s -setcookie bob`

- Compilieren: `c(werkzeug), c(myserver).
- Server starten: `myserver:start().`

###### Client Terminal Starten: `erl -sname c -setcookie bob`

- Compilieren: `c(client).`
- Client starten: `client:start(s@workstation).`    *s@workstation = Nodename*


##Aufgabe 2

###### Nameservice Terminal Starten `erl -sname ns -setcookie bob`

- Compilieren: `c(nameservice), c(werkzeug), c(tools), c(util).`
- Starten: `nameservice:start().`
 
###### Coordinator Terminal Starten `erl -sname c -setcookie bob`

- Compilieren: `c(coordinator).`
- Starten: `coordinator:start(ns@workstation).`    *ns@workstation = Nodename*

###### Starter Terminal Starten `erl -sname s -setcookie bob`

- Compilieren: `c(starter), c(ggt).`
- Starten: `starter:start(1).`      *1 = Nummer des Starters*

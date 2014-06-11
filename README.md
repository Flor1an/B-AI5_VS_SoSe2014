VS_SoSe2014
===========

##Aufgabe 1 (Message of the Day)

###### Server Terminal Starten: `erl -sname s -setcookie bob`

- Compilieren: `c(werkzeug), c(myserver).`
- starten: `myserver:start().`

###### Client Terminal Starten: `erl -sname c -setcookie bob`

- Compilieren: `c(client).`
- starten: `client:start(s@workstation).`    *s@workstation = Nodename*


##Aufgabe 2 (GGT/GCD)

###### Nameservice Terminal Starten `erl -sname ns -setcookie bob`

- Compilieren: `c(nameservice), c(werkzeug), c(tools), c(util).`
- Starten: `nameservice:start().`
 
###### Coordinator Terminal Starten `erl -sname c -setcookie bob`

- Compilieren: `c(coordinator).`
- Starten: `coordinator:start(ns@workstation).`    *ns@workstation = Nodename*

###### Starter Terminal Starten `erl -sname s -setcookie bob`

- Compilieren: `c(starter), c(ggt).`
- Starten: `starter:start(1).`      *1 = Nummer des Starters*


##Aufgabe 3 

###### tbd

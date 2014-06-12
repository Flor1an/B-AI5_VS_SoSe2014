VS_SoSe2014
===========

##Aufgabe 1 (Message of the Day)

###### Server Terminal Starten: `erl -sname s -setcookie bob`

- Kompilieren: `c(werkzeug), c(myserver).`
- starten: `myserver:start().`

###### Client Terminal Starten: `erl -sname c -setcookie bob`

- Kompilieren: `c(client).`
- starten: `client:start(s@workstation).`    *s@workstation = Nodename*


##Aufgabe 2 (GGT/GCD)

###### 2.1 Nameservice Terminal Starten `erl -sname ns -setcookie bob`

- Kompilieren: `c(nameservice), c(werkzeug), c(tools), c(util).`
- Starten: `nameservice:start().`
 
###### 2.2 Coordinator Terminal Starten `erl -sname c -setcookie bob`

- Kompilieren: `c(coordinator).`
- Starten: `coordinator:start(ns@workstation).`    *ns@workstation = Nodename*

###### 2.3 Starter Terminal Starten `erl -sname s -setcookie bob`

- Kompilieren: `c(starter), c(ggt).`
- Starten: `starter:start(1).`      *1 = Nummer des Starters*

###### 2.4 Steuer Terminal Starten `erl -sname x -setcookie bob`
**coordinatorFM** = *Koordinatorname (aus Config)*<br>
**c@workstation** = *Name vom Koordinator Terminal*

**Nach REGISTER**
- `{coordinatorFM,c@workstation} ! step.` Erstellt Ring; Informiert über Nachbarn

**Nach step**
- `{coordinatorFM,c@workstation} ! calc.` Lässt GGT basierend auf einer random Zahl berechnen
- `{coordinatorFM,c@workstation} ! {calc,100}.` Lässt GGT berechnen mit dem Ziel GGT=100

**Jederzeit**
- `{coordinatorFM,c@workstation} ! prompt.` Fragt bei allen GGT's den letzten MI ab
- `{coordinatorFM,c@workstation} ! whats_on.` Fragt bei allen GGT's den letzten STATUS ab
- `{coordinatorFM,c@workstation} ! reset.` Killed alle GGT's und begiebt sich in den REGISTER status
- `{coordinatorFM,c@workstation} ! kill.` Killed alle GGT's und exit sich selber


##Aufgabe 3 

###### tbd

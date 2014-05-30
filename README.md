VS_SoSe2014
===========

##Aufgabe 1

Server Terminal Starten: `erl -sname sss -setcookie bob`

Client Terminal Starten: `erl -sname ccc -setcookie bob`


Compilieren: `c(werkzeug), c(myserver), c(client).`


Server starten: `myserver:start().`

Client starten: `client:start(sss@workstation).`

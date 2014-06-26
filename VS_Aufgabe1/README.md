##Aufgabe 1 (Message of the Day)

###### Server Terminal Starten: `erl -sname s -setcookie bob`

- Kompilieren: `(s@workstation)1> c(werkzeug), c(myserver).`
- starten: `(s@workstation)2> myserver:start().`

###### Client Terminal Starten: `erl -sname c -setcookie bob`

- Kompilieren: `(c@workstation)1> c(client).`
- starten: `(c@workstation)2> client:start(s@workstation).`    *s@workstation = Nodename*
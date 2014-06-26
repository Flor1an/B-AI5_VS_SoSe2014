#!/bin/bash

#./STDMAsniffer_32 225.10.1.2 15000 eth0 -adapt | grep -E 'coll|=='

killall java;

clear;

if [ "$1" == "" ]
	then
		echo "##############################################################################################################################";
		echo "### Kein Testfall angegeben. z.B. 'bash Test.sh 1'                                                                         ###";
		echo "##############################################################################################################################";
		echo "### ./STDMAsniffer_32 225.10.1.2 15000 eth0 -adapt | grep -E 'coll|=='                                                     ###";
		echo "### java -cp . datasource.DataSource <team> <id> | java -jar jar/Team<team>.jar <interface> <host> <port> <class> <offset> ###";
		echo "##############################################################################################################################";
fi

if [ "$1" == "1" ]
	then
		echo "################################################";
		echo "### 5 x 3 Stationen Team 1 bis 3 kein Offset ###";
		echo "################################################";
		./startStations.sh eth0 225.10.1.2 15000 1 5 A 0 2;
		./startStations.sh eth0 225.10.1.2 15000 6 11 A 0 3;
		./startStations.sh eth0 225.10.1.2 15000 12 16 A 0 8;
fi

if [ "$1" == "2" ]
	then
		echo "##########################################";
		echo "###    5 x 3 Stationen Team 1 bis 3    ###";
		echo "### Je Offset 1000, 900, 800, 700, 600 ###";
		echo "##########################################";
		./startStations.sh eth0 225.10.1.2 15000 1 1 A 1000 2;
		./startStations.sh eth0 225.10.1.2 15000 2 2 A 900 2;
		./startStations.sh eth0 225.10.1.2 15000 3 3 A 800 2;
		./startStations.sh eth0 225.10.1.2 15000 4 5 A 700 2;
		./startStations.sh eth0 225.10.1.2 15000 4 5 A 600 2;

		./startStations.sh eth0 225.10.1.2 15000 1 1 A 1000 3;
		./startStations.sh eth0 225.10.1.2 15000 2 2 A 900 3;
		./startStations.sh eth0 225.10.1.2 15000 3 3 A 800 3;
		./startStations.sh eth0 225.10.1.2 15000 4 5 A 700 3;
		./startStations.sh eth0 225.10.1.2 15000 4 5 A 600 3;

		./startStations.sh eth0 225.10.1.2 15000 1 1 A 1000 8;
		./startStations.sh eth0 225.10.1.2 15000 2 2 A 900 8;
		./startStations.sh eth0 225.10.1.2 15000 3 3 A 800 8;
		./startStations.sh eth0 225.10.1.2 15000 4 5 A 700 8;
		./startStations.sh eth0 225.10.1.2 15000 4 5 A 600 8;
fi
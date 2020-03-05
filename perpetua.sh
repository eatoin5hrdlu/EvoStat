#!/bin/bash
cd ~/src/EvoStat
while true; do
    EPID=`pgrep evostat`
    if [ "$EPID" = "" ]; then
	mv web/evostat.txt evostat.txt.`date +"%d-%m-%Y-%H:%M:%S"`
	echo "EvoStat is down. Launching new Evostat" >>evostat.log
	nohup ./evostat 2>&1 >evostat.txt &
    fi
    echo "EvoStat PID is [" $EPID "]"
    sleep 5
done

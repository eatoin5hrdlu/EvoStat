#!/bin/bash
cd /home/peter/src/EvoStat
while true; do
    EPID=`pgrep evostat`
    if [ "$EPID" = "" ]; then
	echo "EvoStat is down. Launching new Evostat" >>evostat.log
	nohup ./evostat 2>&1 >>evostat.log &
    fi
    echo "EvoStat PID is [" $EPID "]"
    sleep 5
done

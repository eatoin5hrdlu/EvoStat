# SWIPL = swipl-win
SWIPL = swipl

camreset: camreset.c
	cc camreset.c -o camreset
	sudo chown root camreset
	sudo chgrp root camreset
	sudo chmod +s camreset

test : plblue.so pace
	${SWIPL} -s pace -g main

wintest : plblue.dll pace
	${SWIPL}-win -s pace -g main

evostat : c.pl binmaker
	${SWIPL} -s c.pl -t "save_evostat"


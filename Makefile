

test : plblue.so pace
	swipl -s pace -g main

wintest : plblue.dll pace
	swipl-win -s pace -g main

evostat : b.pl binmaker
	swipl-win -s binmaker -t "save(evostat)"


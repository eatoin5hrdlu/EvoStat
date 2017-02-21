#!/usr/bin/python -u
import os, sys
import matplotlib.pyplot as plt
import numpy as np

import time
while( os.stat(sys.argv[1]).st_size < 200 ) :
    time.sleep(1)
    print("waiting...")
f = open(sys.argv[1],'r')
list = []
list2 = []
for l in f.readlines() :
    l = l.split()
    list.append(int(l[0]))
    list2.append(int(l[1]))
llen = len(list)
print(np.mean(list))
print(np.mean(list2))
plt.plot(list)
plt.plot(list2)
plt.axis([0,llen,0,100])
plt.ylabel('Levels in EvoStat')
plt.show()

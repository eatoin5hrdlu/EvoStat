#!/usr/bin/python -u
from __future__ import print_function
import os, sys
import matplotlib.pyplot as plt
import numpy as np
import re
import datetime as dt

import time
new = open(sys.argv[1],'r')
current = dt.datetime.now()
outname = "level."+str(current.day)+"_"+str(current.hour)
f = open(outname,'w')
list1 = []
list2 = []
tm1 = []
tm2 = []
for l in new.readlines() :
    m = re.search('levels\(([0-9]+),([0-9]+)\)',l)
    if (m is None):
        m = re.search('temps\(([0-9]+),([0-9]+)\)',l)
        if (m is None):
            continue
        else :
            tm1.append(int(m.group(1))/10) # tenths of degrees C
            tm2.append(int(m.group(2))/10)
    else :
        print(m.group(1)+" "+m.group(2), file=f)
        list1.append(int(m.group(1)))
        list2.append(int(m.group(2)))
llen = len(list1)
print("length of level1 plot "+str(llen))
fig = plt.figure()
ax1 = fig.add_subplot(111)
ax1.set_ylabel('Levels in EvoStat',color='b')
ax1.plot(list1,'b')
ax1.plot(list2,'g')
ax2 = ax1.twinx()
print("length of temperature1 plot "+str(len(tm1)))
ax2.plot(tm1, 'y')
ax2.plot(tm2, 'r')
ax2.axis([0,llen,0,100])
ax2.set_ylabel('Temp Centigrade',color='r')
print("First value = " + str(list1[0]) + " Average 1:" + "{:7.2f}".format(np.mean(list1)) + "   Last value = "+str(list1[-1]))
print("First value = " + str(list2[0]) + " Average 2:" + "{:7.2f}".format(np.mean(list2)) + "   Last value = "+str(list2[-1]))
ax1.axis([0,llen,0,100])
plt.show()
#ax2.set_ylim([0,100])

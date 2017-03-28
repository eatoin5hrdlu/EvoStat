#!/usr/bin/python -u
from __future__ import print_function
for i in range(9) :
    n = i+1
    print("    ",end='')
    for lr in ['R','L'] :
        for fb in ['F', 'B'] :
            print(lr+fb+str(n)+"           ",end='')
    print()
print("      ",end='')
for i in range(4) :
    print(str(i+1)+"            ",end='')
print()
            

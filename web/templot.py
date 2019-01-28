#!/usr/bin/python -u
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import AutoMinorLocator, MultipleLocator, FuncFormatter
import datetime
import sys

maxx = 19   # How many days total
maxy = 38   # Highest temperature of interest
miny = 32   # Lowest Temperature  "

X, Y1, Y2 = np.loadtxt(sys.argv[1], delimiter=',', unpack=True)

fig = plt.figure(figsize=(8, 8))
ax = fig.add_subplot(1, 1, 1, aspect=2)

def minor_tick(x, pos):
    if not x % 1.0:
        return ""
    return "%.2f" % x

ax.xaxis.set_major_locator(MultipleLocator(1.000))
ax.xaxis.set_minor_locator(AutoMinorLocator(4))
ax.yaxis.set_major_locator(MultipleLocator(1.000))
ax.yaxis.set_minor_locator(AutoMinorLocator(10))
ax.xaxis.set_minor_formatter(FuncFormatter(minor_tick))


ax.set_xlim(0, maxx)
ax.set_ylim(miny, maxy)

ax.tick_params(which='major', width=1.0)
ax.tick_params(which='major', length=10)
ax.tick_params(which='minor', width=1.0, labelsize=3)
ax.tick_params(which='minor', length=5, labelsize=3, labelcolor='0.25')

ax.grid(linestyle="--", linewidth=0.5, color='.25', zorder=-10)

ax.plot(X/86400.0, Y2, c=(1.00, 0.25, 0.25), lw=0.2, label="Lagoon", zorder=11)
ax.plot(X/86400.0, Y1, c=(0.25, 0.25, 1.00), lw=0.2, label="Host Culture", zorder=10)

ax.set_title("EvoStat Temperature January 7-26, 2019", fontsize=20, verticalalignment='bottom')
ax.set_xlabel("Time (days)")

# ax.yaxis.tick_right()  # default is left

ax.set_ylabel("Temperature $^\circ$C")


ax.legend()

def text(x, y, text):
    ax.text(x, y, text, backgroundcolor="white",
            ha='center', va='top', weight='bold', color='blue')

color = 'blue'
ax.text(maxx/3, miny-2, "Created "+
        datetime.datetime.now().strftime("%I:%M %p on %B %d, %Y"),
        fontsize=10, ha="left", color='.1')
# Single number for color is greyscale (1.0 is black)
plt.savefig('temperature')


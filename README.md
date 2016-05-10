EvoStat:  A PhageStat to support PACE and PATHE Experiments
====
Software and Hardware for an automated platform for PACE and PATHE experiments.

PATHE (Phage-Assisted Three-Hybrid Evolution) is an enhancement to PACE (Phage Assisted Continuous Evolution) described by Kevin M. Esvelt, Jacob C. Carlson, and David R. Liu in http://www.ncbi.nlm.nih.gov/pubmed/21478873

This project contains Arduino code for a Host CellStat (Turbidostat), Sample Collector, and Lagoons
as well as the server software to provide an on-screen interface to Temperature, Turbidity, Flowrate,
and Sample Collector control and Web interface.

SWI-Prolog based web server, GUI, and Turbidostat control software written mostly in Prolog/Xpce with some Python (e.g. pySerial, for a 
portable serial interface to Arduino) and HTML. Bluetooth interface added as foreign function library to SWI-Prolog currently eliminates the need for pySerial. See github.com/eatoin5hrdlu/plblue 

Running EvoStat on a Linux or Windows machine provides an interface for multiple phageStats with using Bluetooth connected Arduino running host.ino for the Host Cellstat, multiple Bluetooth connected Arduinos for Lagoons (lagoon.ino), and a sample collector: Bluetooth connected Arduino running collector.ino. (Using a WiFi camera and bluetooth modules has reduced apparatus wiring by 90%)

It also contains a web server to provide essentially the same
interface via the URL:  

    http://&lt;machine-name&gt;:8080/web/pathe.pl    


Prerequisite Software
====
- Python2.7
- OpenCV (plus: "apt-get install python-opencv")
- SWI-Prolog with Xpce
- Arduino IDE (Processing)
-
- On Linux add : imagemagick (for conversion of image files)
- apache2-utils: for Authentication (encrypted password) Files
- 

Getting Started
====
Clone the repo:

    git clone https://github.com/eatoin5hrdlu/EvoStat.git

--- Level detection Python/OpenCV

A python program (ipcam.py) uses OpenCV to read the liquid levels of the vessels.
Windows and linux versions of #! in ipcam.py appear at the top of the file
It may be necessary to move the correct one to the top. Or call python:

$ python ipcam.py lagoon (read the lagoon liquid levels)
$ python ipcam.py cellstat (cellstat level)
$ python ipcam.py locate  (show image and write bbox from mouse area selection)

------------------
Create stand-alone executable "evostat" (Prolog saved-state)
$ swipl (linux) or swipl-win
:- [c].
:- save_evostat.

Or to do debugging, start the program by typing:

$ swipl(-win)
:- [c].
:- c.

Then when the interface comes up, you can:
turn off the automatic level sensing with Action->Stop
turn off the PID volume/flow controllers  Action->PIDstop

View the Web Interface with the URL  http://localhost:21847/web/pathe.pl

The unusual port number is from the NetSpeak (WebPhone) Connection Server (no longer in use).
Change it in c.pl to 80 or 8080 for normal web service.

--------------
Changing the Icon on a Prolog saved-state:

             chicon.exe saved.exe myapp.ico myapp.exe
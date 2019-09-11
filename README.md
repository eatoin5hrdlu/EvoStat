EvoStat:  A PhageStat to support PACE and PATHE Experiments
====
Software and Hardware for an automated platform for PACE and PATHE experiments.

PATHE (Phage-Assisted Three-Hybrid Evolution) is our enhancement to PACE (Phage Assisted Continuous Evolution) which was described by Kevin M. Esvelt, Jacob C. Carlson, and David R. Liu in http://www.ncbi.nlm.nih.gov/pubmed/21478873

This project contains Arduino code for a Host CellStat (Turbidostat), Sample Collector, and Lagoons
as well as the server software to provide an on-screen interface to Temperature, Turbidity, Flowrate,
and Sample Collector control and Web interface.  The goal is a remote access, fully automated system with a build cost less than $1000.00. 

SWI-Prolog based web server, GUI, and Turbidostat control software written mostly in Prolog/Xpce with some Python (e.g. pySerial, for a 
portable serial interface to Arduino) and HTML. Bluetooth interface added as foreign function library to SWI-Prolog currently eliminates the need for pySerial. See github.com/eatoin5hrdlu/plblue 

Running EvoStat on a Linux or Windows machine provides an interface for multiple phageStats with Bluetooth connected Arduinos running module.ino (then software defined as Host or Lagoon controller) and a sample collector: Bluetooth connected Arduino running collector.ino. (Using a WiFi camera and bluetooth modules has reduced apparatus wiring by 90%)

A web interface provides a similar interface via the URL:  

    http://<host>:21847/web/pathe.pl


Prerequisite Software
====
- Python2.7
- OpenCV (on Linux add "apt-get install python-opencv")
- SWI-Prolog with Xpce
- Arduino IDE (Processing)
- python-psutil, ffmpeg, octave, (some other stuff, watch this space).
-
- On Linux add : imagemagick (for conversion of image files)
- apache2-utils: for Authentication (encrypted password) Files
- 

Getting Started
====
Clone the EvoStat repo:

    git clone https://github.com/eatoin5hrdlu/EvoStat.git
    
Clone the plblue repo (Prolog interface to Bluetooth communication)

    git clone https://github.com/eatoin5hrdlu/plblue.git
    
    cd plblue
    
    make      ( required packages: libbluetooth-dev and bluez )
    
    make install  (moves the shared library into the ../EvoStat directory
    

--- LEVEL detection Python/OpenCV

A python program (alevel.py) uses OpenCV to read the liquid levels of the vessels.
Windows and linux versions of #! in ipcam.py appear at the top of the file
It may be necessary to move the correct one to the top. Or call python:

$ python level.py 
$ python level.py show -c 30 sample.jpg
OPTIONS: -c N repeat in N seconds (default 90)
         show   (produce debugging output, including processed images)
         <filename>.jpg    (Read the image file instead of the camera /dev/video{0,1,2} )

------------------
Create stand-alone executable "evostat" (Prolog saved-state)
$ swipl (linux) or swipl-win
:- [c].
:- save_evostat.

Or to maintain terminal access for debugging, start with:

$ swipl(-win)
:- [c].
:- c.

When the interface comes up, you can:
turn off the automatic level sensing with Action -> Stop
turn off the PID volume/flow controllers  Action -> PIDstop
Get back to the Prolog prompt with File -> OK

View the Web Interface with the URL  http://localhost:21847/web/pathe.pl

The unusual port numbers are from the ca. 1995 NetSpeak (WebPhone). My legacy, no longer in use.
Change it in c.pl to 80 or 8080 to appear as a normal web server

--------------
To changing the Icon on a Prolog saved-state on Windows:

    chicon.exe saved.exe myapp.ico myapp.exe


###########################
Procedure for operating the EvoStat:

Sterilize Supply and Drain Valves with 1M NaOH

Install Valves

Autoclave Cellstat and Lagoon vessels

Autoclave Nutrient/Inducers in conainers

Install supply vessels on top of EvoStat and connect to supply valves

Connect supply valves to Cellstat

Open bypass valve to fill Cellstat with Nutrient

Close bypass valve when Cellstat Level is at desired set point (85%)

Inject host cells into Cellstat when target temperature is reached.

Normal flow control begins when OD target has been reached.

Infect lagoons with phage.

Regular texts will be sent to report EvoStat status.
	Each text includes the URL to EvoStat status/control page.
Exceptional texts will be sent if:
	1) temperature / turbidity / level / flow out of range
	2) leak has been detected



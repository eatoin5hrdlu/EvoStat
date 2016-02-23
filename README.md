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

    http://&lt;machine-name&gt;:8080/evostat.pl    


Prerequisite Software
====
- Python2.7
- OpenCV (plus: "apt-get install python-opencv")
- SWI-Prolog with Xpce
- Arduino IDE (Processing)

Getting Started
====
Clone the repo:

    git clone https://github.com/eatoin5hrdlu/EvoStat.git

Run the app:

    cd app
    python levelapp.py

Using the controls, line up the lagoons and turbidostat in the camera frame
with the corresponding lines being  (blob detection being added to automate this)

Controls
====
* z  - read one frame and find levels and display
* r - read, find levels and display for 20 seconds.
* 0<nl>   Select the turbidostat as current vessel
* {1,2,3,4 for lagoons)
* <arrow keys> to move up/down/left/right, or:
* h  - move to the left
* j  - move down
* k  - move up
* l  - move right
* t -make region taller
* s - make region shorter
* f - make region fatter
* d - diminish (make region thinner)
* x - exits the program
---
Making a Prolog executable (saved-state)

:- [load].

main :-      pce_main_loop(main).

main(Argv) :-  start application here, using passed arguments from Argv

save_evostat :-
        pce_autoload_all,
        pce_autoload_all,
        qsave_program(evostat,
                      [ emulator(swi('bin/xpce-stub.exe')),
                        stand_alone(true),
                        goal(main)
                      ]).

-------
Changing the Icon on a Prolog saved-state:

             chicon.exe saved.exe myapp.ico myapp.exe
// Sample Collector and Drain Valves

#define P(x)  Serial.print(x)
#define PL(x) Serial.println(x)

int overflow_sensor[2] = { A0, A7 };
int overflow_valve[2]  = { A1, A2 };

char id = 'a'; // s is default for supply

#define EOT "end_of_data."

void printInterface()
{
  PL(F("i( sampler, ebutton,\n\
      [ rw(up, int:=90,   \"Update every 90 seconds\"),\n\
	rw(al, int:=10,   \"Aliquot time in seconds\"),\n\
	rw(ns, int:=24,   \"Number of samples\"),\n\
	rw(ts, int:=3600, \"Time to next sample\"),\n\
	rw(pd, int:=90,   \"Drain Timing Interval\"),\n\
	rw(gt, int:=3600, \"Group Time\"),\n\
	rw(gn, int:=1,    \"Group Number\"),\n\
        rw(v0, int:=1000, \"Host Drain Valve Timing\"),\n\
        rw(v1, int:=1000, \"Lagoon1 Drain Valve Timing\"),\n\
        rw(v2, int:=1000, \"Lagoon2 Drain Valve Timing\"),\n\
        rw(v3, int:=1000, \"Lagoon3 Drain Valve Timing\"),\n\
        rw(v4, int:=1000, \"Lagoon4 Drain Valve Timing\")\n\
       ])."));
}

      
// Requires Arduino with:
// Two interrupt pins: Home photo-interruptor(pin 2) and Encoder A input (pin 3)
// Two inputs for Encoder B (pin 4) and sample platform endstop (pin 6).
// Four outputs for the phases of the sample platform stepper (8-11)
// Five outputs for the drains of Lagoons 1-4 and the CellStat (A1-A5).
// 
// 1) Set Start Time
// 2) Set number of samples and times
//
//
// Pin numbers currently for Ardweeny
//
// Maxon Motor Encoder 8-pin header pinout
// (Red stripe) Pin 1: Motor (+)
//              Pin 2: Shield
//              Pin 3: Channel B
//              Pin 4: Vcc Enc (5V)
//              Pin 5: Channel A
//              Pin 6: Gnd Enc
//              Pin 7: Shield
//              Pin 8: Motor (-)
//
#define digitalPinToInterrupt(p)  (p==2?0:(p==3?1:(p>=18&&p<=21?23-p:-1)))
#define NUM_VALVES 5
#define HOSTFULL   A0   /* Analog liquid level sensor */
#define LAGOONFULL A2   /* Analog liquid level sensor */
//
// The following numbers work for the 12-volt motor supply
// With four tubes to compress, 5-volts wasn't enough torque
// These numbers should be stored in EEPROM and re-programmable
//

#define FULL_POWER   240
#define MEDIUM_POWER 180
#define LOW_POWER    120
#define PWM_OFF        0

#define ENCODER_180  400  // PWM Power and this are GearMotor dependent

#define ARDWEENY 1
#ifdef ARDWEENY
const int drivePin = 5; // DC motor, PWM with three power levels
const int closePin = 2; // PIN with ISR for photo-interrupter
const int countPin = 3; // PIN with ISR for Gearmotor Encoder Phase A
const int phaseB   = 4; // PIN                        Encoder Phase B

const int ENDSTOP    =  6;
const int SAMPLE_SPACE = 54;
const int GROUP_SPACE = 100;

// Stepper Phases: forward (1-3-2-4) reverse (4-2-3-1)
const int P1 =  8;
const int P2 =  9;
const int P3 = 10;
const int P4 = 11;

#endif

const int DEFAULT_VALVECYCLE = 60000UL;
const int DEFAULT_SAMPLES    = 24;
const int DEFAULT_SAMPLETIME = 3600UL;
const int DEFAULT_ALIQUOT    = 500UL;

#define EOT     "end_of_data."
#include <EEPROM.h>
const boolean debug     = false;
      boolean firstTime = true;

// Set global 'RomAddress' and address will be bumped
// by successive calls to moveData( SAVE|RESTORE, size, ptr)
const int      SAVE   = 1;
const int   RESTORE   = 0;
      int   RomAddress;
//
//
volatile int state;

const int STARTUP  = 0;
const int CLOSED   = 1;
const int COUNTING = 2;
const int OPENED   = 3;
const int CLOSING  = 4;

char *statename[] = {
	"STARTUP",
	"CLOSED",
	"COUNTING",
	"OPENED",
	"CLOSING",
};
// Interrupt variables and service routine (ISR)
volatile boolean closedPosition;                               // BOOLEAN 
void             closed_position() { closedPosition = true; }  // photointerrupter

volatile int     ENCODER_count;
int              last_ENCODER_count;

void encoderA()                   // Interrupt on Phase A  0 -> 1
{
//	if (digitalRead(phaseB))  // If (Phase B is HIGH)
	   ENCODER_count++;           //        rotation is CCW
}

unsigned long openInterval = 1000UL;          // 3 seconds
 
unsigned long now;
unsigned long lastTime;
unsigned long lastSampleTime;
unsigned long onTime;
unsigned long cycleTime;
unsigned long valveCycle;
unsigned long lastValveCycle;


// Platform Control

int sampleTime;    // Time between samples
int sampleNum;     // Total number of samples to take
int groupNum;      // Number of samples per group
int groupTime;     // Time between groups
int aliquot;       // Milliseconds for sample taking
unsigned int valveInterval;
int valveTime[6]; // Valve open time in milliseconds per interval
int valvePin[6]; // Valve open time in milliseconds per interval
int updateTime;

int sampleCountdown; // Set to SampleNum after RESET
unsigned long int sampleTimeMS;


void printHelp(void)
{
	Serial.println("a(value,'aliquot - extraction in msec').");
	Serial.println("c('clear - reset platform/start sampling').");
	Serial.println("d('dump (print) settings').");
	Serial.println("e(value,'time between sample groups').");
	Serial.println("g(value,'number of samples in group').");
	Serial.println("h('help - print this command menu').");
	Serial.print("i(");Serial.print(valveTime[0]);Serial.println(",'set lagoon1 drain time(ms)').");
	Serial.print("j(");Serial.print(valveTime[1]);Serial.println(",'set lagoon2 drain time(ms)').");
	Serial.print("k(");Serial.print(valveTime[2]);Serial.println(",'set lagoon3 drain time(ms)').");
	Serial.print("l(");Serial.print(valveTime[3]);Serial.println(",'set lagoon4 drain time(ms)').");
	Serial.print("m(");Serial.print(valveTime[4]);Serial.println(",'set cellStat drain time(ms)').");
	Serial.print("n(");Serial.print(sampleNum);Serial.println(",'total number of samples').");
	Serial.print("p("); Serial.print(valveInterval);Serial.println(",'set drain timing interval').");
	Serial.print("t("); Serial.print(sampleTime); Serial.println(",'time between samples').");
	Serial.println("s('save current settings').");
	Serial.println("r('restore stored settings').");
	Serial.println("z('zero EEPROM - defaults restored after reset').");
}

// BEGIN DEFAULT CALIBRATION PARAMETERS
// Values will be read from EEPROM, after being saved with the 's' command.
// Values below are the defaults before they are saved to EEPROM or 
// after using the 'z' command to zero the EEPROM.


const int MT = 7;  // mS delay between stepper motor phase shifts

void reset()
{
int toolong = 100;
	if (debug) Serial.print("reset(start).");
	while(digitalRead(ENDSTOP) && toolong--)
	{
		reverse(10);
		respondToRequest();
	}
	sampleCountdown = sampleNum;  // Reset to start sample count
	if (debug) {
		if (toolong < 1) Serial.println("reset(timeout).");
		Serial.println("reset(finished).");
	}
}

void setvpin(int v,int p)
{
  pinMode(p,OUTPUT);
  digitalWrite(p,0);
  valvePin[v] = p; 
}

int ctr;
void setup()
{
int i;
	Serial.begin(9600);
	
	// Sampler Pins
//	pinMode(drivePin, OUTPUT);
	analogWrite(drivePin, PWM_OFF);

	pinMode(closePin, INPUT);
	pinMode(countPin,  INPUT);
	pinMode(phaseB,    INPUT);

        // Five Drain Valves
	setvpin(0,A1);
	setvpin(1,A2);
	setvpin(2,A3);
	setvpin(3,A4);
	setvpin(4,A5);
	setvpin(5,A6);
	
	closedPosition = false;
	lastTime = millis();
	ctr = 0; 
	state = STARTUP;

	// Platform Control

	pinMode(P1, OUTPUT);	digitalWrite(P1,    0);
	pinMode(P2, OUTPUT);	digitalWrite(P2,    0);
	pinMode(P3, OUTPUT);	digitalWrite(P3,    0);
	pinMode(P4, OUTPUT);	digitalWrite(P4,    0);
	pinMode(ENDSTOP,        INPUT_PULLUP);

	RomAddress  = 0;
	if (EEPROM.read(0) == 0 or EEPROM.read(0) == 255) // firsttime
	{
		if (debug)
		   Serial.print("Setting defaults in EEPROM.");

		sampleTime = DEFAULT_SAMPLETIME;// Seconds between samples
		sampleNum  = DEFAULT_SAMPLES;   // Two 96-well plates lengthwise
		groupNum   = 12;                // Number of samples per group
		groupTime  = 0;                 // Time between groups
		aliquot = DEFAULT_ALIQUOT;      // SAMPLE SIZE in mSec
		for (i=0; i< 6; i++) valveTime[i] = 800;
		valveInterval = DEFAULT_VALVECYCLE;
		saveRestore(SAVE);
	}
	saveRestore(RESTORE);
	sampleTimeMS = sampleTime*1000UL;  // Sampling interval in milliseconds
	openInterval = aliquot;
	if (debug)
		printHelp();
	lastSampleTime = millis(); // Wait long interval for first sample
	reset();
}

void forward(int steps)
{
	for(int i=0; i<steps; i++) {
		digitalWrite(P1, 1);
		delay(MT);
		digitalWrite(P4, 0);
		delay(MT);
		digitalWrite(P3, 1);
		delay(MT);
		digitalWrite(P1, 0);
		delay(MT);
		digitalWrite(P2, 1);
		delay(MT);
		digitalWrite(P3, 0);
		delay(MT);
		digitalWrite(P4, 1);
		delay(MT);
		digitalWrite(P2, 0);
		delay(MT);
	}
	digitalWrite(P4, 0);
}

void reverse(int steps)
{
	for(int i=0; i<steps; i++) {
		digitalWrite(P4, 1);
		delay(MT);
		digitalWrite(P1, 0);
		delay(MT);
		digitalWrite(P2, 1);
		delay(MT);
		digitalWrite(P4, 0);
		delay(MT);
		digitalWrite(P3, 1);
		delay(MT);
		digitalWrite(P2, 0);
		delay(MT);
		digitalWrite(P1, 1);
		delay(MT);
		digitalWrite(P3, 0);
		delay(MT);
	}
	digitalWrite(P1, 0);
}


void flash(void)
{
        digitalWrite(13,1);
	delay(300);
	digitalWrite(13,0);
}

void moveData(int op, int size, byte *loc)
{
	for(int i=size;i>0;i--)
		if (op == SAVE)
			EEPROM.write(RomAddress++,*loc++);
		else
			*loc++ = EEPROM.read(RomAddress++);
}

void saveRestore(int op)
{
	int i;
	RomAddress = 0;
	if (op == RESTORE and (EEPROM.read(0) == 0 or EEPROM.read(0) == 255))
	{
	  if (debug)
	    Serial.println("Cannot RESTORE from uninitialized zero EEPROM");
	  return;
	}
	moveData(op, sizeof(int), (byte *)&sampleTime );
	moveData(op, sizeof(int), (byte *)&sampleNum );
	moveData(op, sizeof(int), (byte *)&groupNum );
	moveData(op, sizeof(int), (byte *)&groupTime );
	moveData(op, sizeof(int), (byte *)&aliquot );
	moveData(op, sizeof(int), (byte *)&valveInterval );
	moveData(op, 6*sizeof(int), (byte *)&valveTime );
}

void dump(void)    // Print configuration settings
{
  int i;
  Serial.print("t "); Serial.println(sampleTime);
  Serial.print("n "); Serial.println(sampleNum);
  Serial.print("g "); Serial.println(groupNum);
  Serial.print("a(ms) "); Serial.println(aliquot);
}

void respondToRequest(void)
{
	String is = "";
	while (Serial.available() > 0)  // Read a line of input
	{
		int c  = Serial.read();
		if ( c < 32 ) break;
		is += (char)c;
		if (Serial.available() == 0) // It is possible we're too fast
			delay(100);
	}
	if ( is.length() > 0 )  {   // process the command
		int value = 0;
		if (is.length() > 2) {
			value = atoi(&is[2]);
			if (value == 0) value = 1;
		}
		process(is[0], is[1], value);
	}
}

void printTermInt(char *functor, int arg)
{ Serial.print(functor); Serial.print("(");Serial.print(arg);Serial.println(")."); }

void printTermUInt(char *functor, unsigned int arg)
{ Serial.print(functor); Serial.print("(");Serial.print(arg);Serial.println(")."); }

void printTerm2Int(char *functor, int arg1, int arg2)
{ 
  Serial.print(functor); Serial.print("(");
  Serial.print(arg1);Serial.print(",");
  Serial.print(arg2);Serial.println(")."); 
}
void printTermChar(char *functor, char arg)
{ Serial.print(functor); Serial.print("(");Serial.print(arg);Serial.println(")."); }

unsigned long int
elapsed(unsigned long int cur_time, unsigned long int prev_time)
{
   if (cur_time < prev_time)   // handle rollover
      return( cur_time - *( (long int *)&prev_time ) );
   else
      return ( cur_time - prev_time );
}

void process(char c, char c2, int value)
{
unsigned long time_left;
unsigned long time_since;
unsigned int temp;
int i;
int vnum;
char vn[3];

  vn[0] = 'v';
  vn[1]= c2,
  vn[2] = 0;
  vnum = (int) c2 -'0';

	switch(c) {
		case 'a': /* al for aliquot */
		     if (value == 0)
		     	printTermInt("al",aliquot);
		     else  {
			aliquot = value;
			openInterval = aliquot;
		     }
		     break;
		case 'c':
			reset();
			break;
		case 'd':
			dump();
			break;
		case 'g':
		     switch (c2) {
		     	    case 't':
			    	 if (value == 0)
				    printTermInt("et",groupTime);
				 else
				    groupTime = value;
			    break;
			    case 'n':
			    	 if (value == 0)
				    printTermInt("gt",groupNum);
				 else
				    groupNum = value;
			    break;
			    default:
				printTermInt("e",(int)c2);
		     }
		     break;
		case 'h':
			printHelp();
			break;
		case 'i':
		     if (c2 == 'd')
		     	printTermInt("id",id);
		     else
		        printInterface();
		     break;
		case 'v':
		     if (vnum<0 || vnum > NUM_VALVES-1)
		     	printTermInt("e",(int)c2);
		     else {
			if (value == 0)
			   printTermInt(vn,valveTime[vnum]);
			else
			   valveTime[vnum] = value;
		     }
		     break;
		case 'l': /* return liquid sensor values */
			Serial.print(value);
			Serial.print(" ");
			Serial.println(overflow_sensor[value-1]);
			printTermInt("l",analogRead(overflow_sensor[value-1]));
			break;
		case 'n': /* ns for number of samples */
			if (value == 0) printTermInt("ns",sampleNum);
			else 		sampleNum = value;
			break;
		case 'r' :
			saveRestore(RESTORE);
			break;
		case 'p': /* pd for Period Drain Timing Interval */
			if (value == 0) printTermUInt("pd",valveInterval);
			else 		valveInterval = value;
			break;
		case 't' :  // ts for Time to next Sample
		     if (c2 == 's') {
		     	if (value == 0) {
			   time_since = elapsed( millis(), lastSampleTime );
			   if (time_since < sampleTimeMS)
			      time_left = sampleTimeMS - time_left;
			   else
			      time_left = 0;
			   temp = (int) ( time_left/1000UL );
			   printTermInt("ts", temp);
			}
			else 
			     sampleTime = value;
		      }
		      else
			printTermChar("e",(int)c2);
		      break;
		case 'u' :
		     	if (value == 0) {
			   printTermInt("up", updateTime);
			}
			else 
			   updateTime = value;
			break;
		case 's' :
			saveRestore(SAVE);
			break;
		case 'z' :
			EEPROM.write(0,0);
			printTermInt("z",0);
			break;
		default :
			printTermChar("e",(int)c);
	}
        Serial.println(EOT);
}

void checkSample(boolean ok) { // Check Sampling State Machine

       // The value of lastTime is usually updated just before the STATE change
       // so it indicates how long we've been in the current STATE
       
	ctr++; // A counter so we don't flood the output with debug msg
	if (debug and (ctr % 30000 == 0)) Serial.println(statename[state]);

	switch(state) {

	case STARTUP:
		attachInterrupt(digitalPinToInterrupt(closePin),closed_position,RISING);
                closedPosition = false;
		firstTime = true;
		lastTime = millis();
		state = CLOSING;
		if (debug) {
				Serial.println("STATE CLOSING");
				Serial.println(digitalRead(closePin));
		}
		analogWrite(drivePin, FULL_POWER);
		break;
	case CLOSING:
		if ( closedPosition && digitalRead(closePin) ) {  // BACK IN HOME POSITION
			if (debug) Serial.println("HOME");
			analogWrite(drivePin, PWM_OFF);
			detachInterrupt(digitalPinToInterrupt(closePin));
			if  (firstTime) {
				firstTime = false;
				lastSampleTime = millis(); // Reset Time
			} else {
				if (debug) Serial.print("Moving platform...");
				sampleCountdown--;         // Decrement Sample-count
				if (sampleCountdown > -1)
				{
					if ((sampleCountdown % groupNum) == 0) {
					   if (debug)
					      Serial.println("Forward Group Spacing");
					   forward(GROUP_SPACE);
					} else {
				           if (debug)
					      Serial.println("Forward Sample Spacing");
					   forward(SAMPLE_SPACE); 
					}
				}
				if (debug) Serial.println(sampleCountdown);
			}
			lastSampleTime = millis(); // Reset Time
			lastTime = millis();
			state = CLOSED;
		}
		break;
	case CLOSED:
		if (sampleCountdown < 1) return;
		now = millis();
		if (ok && ( now > lastSampleTime + sampleTimeMS))  // Time to sample
		{
			if (debug) Serial.println("Attach encoder");
			attachInterrupt(digitalPinToInterrupt(countPin),encoderA,RISING);
			ENCODER_count = 0;
			analogWrite(drivePin, FULL_POWER);
			lastTime = now;
			state = COUNTING;
		}
		break;
	case COUNTING:
		if (debug and (ctr % 1000 == 0)) { // set faster (1000) with motor engaged
		   if (debug)
		      Serial.println(ENCODER_count);
		   if (ENCODER_count != last_ENCODER_count)
                   {
			if (debug)
			   Serial.print("  Encoder : ");
			last_ENCODER_count = ENCODER_count;
			if (debug) {
			   Serial.print(last_ENCODER_count);
			   Serial.print("        boolean: ");
			   Serial.println(closedPosition);
			}
		   }
		   else if (debug) Serial.print(".");
		}
		if (ENCODER_count > (ENCODER_180-45)) {
			analogWrite(drivePin, PWM_OFF);
			if (debug)
			   Serial.println("Detach encoder");
			detachInterrupt(digitalPinToInterrupt(countPin));
			lastTime = millis();
			state = OPENED;
			if (debug)
			   Serial.print(statename[state]);
		}
		else if (ENCODER_count > ENCODER_180 - 140) // Slow on approach
		     analogWrite(drivePin, LOW_POWER);
		else if (ENCODER_count > ENCODER_180 - 200) 
		     analogWrite(drivePin,MEDIUM_POWER);
		break;
	case OPENED:
		now = millis();
		if (now > lastTime + openInterval)      // TIME TO CLOSE
		{
		  if (debug) Serial.print("time to close");
		  closedPosition = false;
		  attachInterrupt(digitalPinToInterrupt(closePin),closed_position,RISING);
		  analogWrite(drivePin, FULL_POWER);
		  lastTime = now;
		  state = CLOSING;
		}
		break;
	}
// NOT NEEDED IF HARDWARE IS WORKING CORRECTLY
	now = millis();
	if (ok && ( now > lastSampleTime + sampleTimeMS + 10000))
	     lastSampleTime = now;
}

// Returns true if all valves are closed (so sampling would be okay)
// which means its okay to sample
boolean checkValves()
{
int i;
boolean allclosed = true;
unsigned long elapsed;
unsigned long now = millis();
	elapsed = now - lastValveCycle;
	for(i=0;i<NUM_VALVES;i++) {
	 	if (digitalRead(valvePin[i])) {  // Valve is open
		   allclosed = false;
		   if (valveTime[i] < (int)elapsed)
		      digitalWrite(valvePin[i],0);
		}
	}
	if (elapsed > (unsigned long)valveInterval)
	{
		if (debug)
	           Serial.println("starting valve cycle");
		lastValveCycle = now;
		for(i=0;i<NUM_VALVES;i++) {
		    if (valveTime[i] > 0)
		       digitalWrite(valvePin[i],1);
		}
	}
	return allclosed;
}

void checkOverflow(void)
{
static int overflow[2]        = { 0, 0 };
static int overflow_value[2]  = { 600, 600 };
int who;

       for (who=0; who<2; who++)
       {
        if (debug and (ctr % 1000 == 0))
	{
	       	  Serial.print(who);
	       	  Serial.print(" <-who  val-> ");
	       	  Serial.println(analogRead(overflow_sensor[who]));
        }
	if (overflow[who] && analogRead(overflow_sensor[who]) > overflow_value[who] + 100)
	{
		overflow_value[who] = max(overflow_value[who] + 100, 400);
		overflow[who] = 0;
	}
	else if (!overflow[who] && analogRead(overflow_sensor[who]) < overflow_value[who])
	{
		overflow[who] = 1;
		overflow_value[who] = analogRead(overflow_sensor[who]);
	}
	else
	{
		if (analogRead(overflow_sensor[who]) > overflow_value[who] + 100)
		   overflow_value[who] = max(overflow_value[who] + 100, 400);
	}
	if (overflow[who])
	{
		Serial.println("opening valve to prevent overflow");
		digitalWrite(overflow_valve[who], 1); /* scheduler will close */
	}
	else if (overflow_value[who]>850)
	{
		Serial.print(overflow_value[who]);
		Serial.print("  Is it closed? ");
		Serial.println(digitalRead(overflow_valve[who]));
	}
       }
}

void loop()
{
	// checkSample will only start taking a sample
	// when checkValves() returns true ( valves are all closed ).

	checkSample( checkValves() );
	checkOverflow();
	respondToRequest();

}

// Sample Collector and Drain Valves
// Requires Arduino with:
// Two interrupt capable input pins for Home position photo-interruptor(pin 2) and Encoder A input (pin 3)
// Two additional input pins for Encoder B (pin 4) and the sample platform endstop (pin 6).
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

const int DEFAULT_SAMPLES    = 24;
const int DEFAULT_SAMPLETIME = 6;
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
int valveInterval;
int valveTime[5]; // Valve open time in milliseconds per interval
int valvePin[5]; // Valve open time in milliseconds per interval

int sampleCountdown; // Set to SampleNum after RESET
unsigned long int sampleTimeMS;

void printHelp(void)
{
	Serial.println("a(value,'aliquot - sample extraction in milliseconds').");
	Serial.println("c('clear - reset platform/start sampling').");
	Serial.println("d('dump (print) settings').");
	Serial.println("e(value,'time between sample groups').");
	Serial.println("g(value,'number of samples in group').");
	Serial.println("h('help - print this command menu').");
	Serial.print("i(");Serial.print(valveTime[0]);Serial.println(",'set lagoon1 drain time(ms)').");
	Serial.print("j(");Serial.print(valveTime[1]);Serial.println(",'set lagoon1 drain time(ms)').");
	Serial.print("k(");Serial.print(valveTime[2]);Serial.println(",'set lagoon1 drain time(ms)').");
	Serial.print("l(");Serial.print(valveTime[3]);Serial.println(",'set lagoon1 drain time(ms)').");
	Serial.print("m(");Serial.print(valveTime[4]);Serial.println(",'set cellStat drain time(ms)').");
	Serial.println("n(value,'total number of samples').");
	Serial.println("p(value,'set drain timing interval').");
	Serial.println("t(value,'time between samples').");
	Serial.println("s('save current settings').");
	Serial.println("r('restore stored settings').");
	Serial.println("z('zero all EEPROM settings - defaults will be restored on next reset').");
}

// BEGIN DEFAULT CALIBRATION PARAMETERS
// Values will be read from EEPROM, after being saved with the 's' command.
// Values below are the defaults before they are saved to EEPROM or 
// after using the 'z' command to zero the EEPROM.


const int MT = 7;  // mS delay between stepper motor phase shifts

void reset()
{
	if (debug) Serial.print("Starting reset (please wait)...");
	while(digitalRead(ENDSTOP))
	{
		if (debug)
		   Serial.println(digitalRead(ENDSTOP));
		reverse(10);
	}
	sampleCountdown = sampleNum;  // Reset to start sample count
	sampleTimeMS = sampleTime*1000UL;  // Sampling interval in milliseconds
	if (debug) Serial.println("finished reset.");
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
	pinMode(A1,OUTPUT);digitalWrite(A1,0);
	pinMode(A2,OUTPUT);digitalWrite(A2,0);
	pinMode(A3,OUTPUT);digitalWrite(A3,0);
	pinMode(A4,OUTPUT);digitalWrite(A4,0);
	pinMode(A5,OUTPUT);digitalWrite(A5,0);
	valvePin[0] = A1;
	valvePin[1] = A2;
	valvePin[2] = A3;
	valvePin[3] = A4;
	valvePin[4] = A5;
	
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
	saveRestore(RESTORE);
	if (EEPROM.read(0) == 0 or EEPROM.read(0) == 255) // firsttime
	{
		if (debug)
		   Serial.print("Setting defaults in EEPROM.");

		sampleTime = DEFAULT_SAMPLETIME;// Seconds between samples
		sampleNum  = DEFAULT_SAMPLES;   // Two 96-well plates lengthwise
		groupNum   = 12;                // Number of samples per group
		groupTime  = 0;                 // Time between groups
		aliquot = DEFAULT_ALIQUOT;      // SAMPLE SIZE in mSec
		for (i=0; i< 5; i++) valveTime[i] = 3000;
		valveInterval = 10000;
		saveRestore(SAVE);
	}
	openInterval = aliquot;
	if (debug)
		printHelp();
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
	moveData(op, 5*sizeof(int), (byte *)&valveTime );
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
		if (is.length() > 2)
			value = atoi(&is[1]);
		process(is[0], value);
	}
}

void printTermInt(char *functor, int arg)
{ Serial.print(functor); Serial.print("(");Serial.print(arg);Serial.println(")."); }

void printTerm2Int(char *functor, int arg1, int arg2)
{ 
  Serial.print(functor); Serial.print("(");
  Serial.print(arg1);Serial.println(",");
  Serial.print(arg2);Serial.println(")."); 
}
void printTermChar(char *functor, char arg)
{ Serial.print(functor); Serial.print("(");Serial.print(arg);Serial.println(")."); }

void process(char c, int value)
{
unsigned long time_left;
int i;
	switch(c) {
		case 'a':
			aliquot = value;
			openInterval = aliquot;
			break;
		case 'c':
			reset();
			break;
		case 'd':
			dump();
			break;

		case 'e':
			groupTime = value;
			break;
		case 'g':
			groupNum = value;
			break;
		case 'h':
			printHelp();
			break;
		case 'i': 
		     if (debug) printTerm2Int("valve",0,value);
		     valveTime[0] = value; break;
		case 'j': 
		     if (debug) printTerm2Int("valve",1,value);
		     valveTime[1] = value; break;
		case 'k': 
		     if (debug) printTerm2Int("valve",2,value);
		     valveTime[2] = value; break;
		case 'l': 
		     if (debug) printTerm2Int("valve",3,value);
		     valveTime[3] = value; break;
		case 'm': 
		     if (debug) printTerm2Int("valve",4,value);
		     valveTime[4] = value; break;

		case 'n':
			sampleNum = value;
			break;
		case 'r' :
			saveRestore(RESTORE);
			break;
		case 'p': valveInterval = value; break;
		
		case 't' :        // Set Value or Report Time to next Sample
		     	if (value == 0) {
			   time_left = sampleTimeMS - (millis()-lastSampleTime);
			   printTermInt("time", (int)(time_left/1000));
			}
			else 
			     sampleTime = value;
			break;
		case 's' :
			saveRestore(SAVE);
			break;
		case 'z' :
			int i;
			for(i=0;i<5*sizeof(int);i++) EEPROM.write(i,0);
			Serial.println("EEPROM ZEROED");
			delay(4000);
			break;
		default :
			printTermChar("ignored",c);
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
				lastSampleTime = millis(); // Reset Time
			}
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
}

boolean checkValves()  // Returns true if all valves are closed (so sampling would be okay)
{
int i;
boolean allclosed = true;
unsigned long elapsed;
unsigned long now = millis();
	elapsed = now - lastValveCycle;
	for(i=0;i<5;i++) {
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
		for(i=0;i<5;i++) {
		    if (valveTime[i] > 0)
		       digitalWrite(valvePin[i],1);
		}
	}
	return allclosed;
}

void loop()
{
	// checkSample will only start taking a sample
	// when checkValves() returns true ( valves are all closed ).


	checkSample( checkValves() );
	respondToRequest();

}

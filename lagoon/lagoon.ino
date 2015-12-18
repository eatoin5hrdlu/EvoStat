/*
 * Lagoon controller
 *
 * 1) Create valve controller
 * 2) Accept commands from main computer to:
 *     a) adjust timings
 *     b) control meniscus light
 *     c) set auto/manual temperature control
 *     d) set auto/manual flow control
 * 3) Check temperature and manage lagoon heater
 */
#include "param.h"

#include <Servo.h>
Servo myservo;

int calibration;

void swrite(int val) {
     if (calibration != 2) {  // Do not enable(LOW) when calibrating outflow
	digitalWrite(VALVEENABLE,0);
	delay(150);
     }
     myservo.write(val);
     delay(150);
     digitalWrite(VALVEENABLE,1);
}

#include "valve.h"        // Includes param.h (change constants there)
VALVE    valve = VALVE(NUM_VALVES+1, VALVEPIN);      // 5-position valve on pin 9


#include "temperature.h" 
TEMPERATURE temp = TEMPERATURE(0);  // Analog Temperature on pin A0

boolean auto_temp;   // Automatically control Heater
boolean auto_valve;  // Automatically control Valves
boolean auto_mixer;  // Automatically cycle mixer

/* EEPROM SAVE AND RESTORE OF ID AND CALIBRATION CONSTANTS */

#ifdef STANDALONE
#include "EEPROM.h"
int RomAddress  = 0;

int mixerspeed;
byte id = 'z'; // Zeno = unassigned, by default
float target_temperature;

int interval;   // Variable to keep track of the time
int reading[10];

// Keep temperature within 0.5 degree C

void checkTemperature()
{
float t = temp.celcius();
	if (t < target_temperature)        digitalWrite(HEATER,1);
	if (t > target_temperature + 0.5)  digitalWrite(HEATER,0);
}

// 'RomAddress' global will be bumped by successive
// calls to moveData( SAVE|RESTORE, size, ptr)

void moveData(int op, int size, byte *loc)
{
	for(int i=size;i>0;i--)
		if (op == SAVE)
			EEPROM.write(RomAddress++,*loc++);
		else
			*loc++ = EEPROM.read(RomAddress++);
}

char *saveRestore(int op)
{
	RomAddress = 0;
	moveData(op, 1, &id);
	moveData(op, sizeof(float),      (byte *) &target_temperature);
	moveData(op, sizeof(int),        (byte *) &mixerspeed);
	moveData(op, sizeof(int),                valve.getOutflowms());
	moveData(op, (NUM_VALVES+1)*sizeof(int), valve.getTimes());
	moveData(op, (NUM_VALVES+1)*sizeof(byte),valve.getAngles());
	if (op == SAVE) return("save.");
	else            return("restore.");
}
#endif

/* Commands:
 *  a0 :    Auto modes off
 *  a1 :    Auto modes on
 *  cN :    Calibration modes (0,1,2)
 *  dNaa :  Set angle for valve position N
 *   EXAMPLE  Set angle for 0th valve position at 10 degrees (rather than 0)    "d010"
 *   EXAMPLE  Set angle for 4th valve position at 178 degrees (rather than 180) "d4178"
 *  h0 :    Heater off
 *  h1 :    Heater on
 *  l0 :    Meniscus light off
 *  l1 :    Meniscus light on
 *  m0 :    Mixing motor off
 *  m1 :    Mixing motor on
 *  ph :    Print Help (this list of commands)    
 *  pN :    Go to Valve position N, auto_valve mode off
 *  r  :    Restore settings from EEPROM
 *  tNNNN:  Set target temperature in tenths of degrees  371 = 37.1C
 *  t  :    Get current temperature
 *
 *  n  : Normal (run mode) : calibration off, auto modes on
 */

void printHelp(void)
{
	Serial.println("cmd(a,[0,1],'set auto modes off/on').");
	Serial.println("cmd(c,[0,1,2],'Calibrate modes(0=normal,1=inflow,2=outflow').");
	Serial.println("cmd(d,[0,1,2,3,4],[45,90,135,180],'set angle:(0-180) for Nth valve position').");
	Serial.println("cmd(h,[0,1],'heater off/on auto_temp off').");
	Serial.println("cmd(l,[0,1],'light off/on').");
	Serial.println("cmd(m,[0,1],'mixer off/on').");
	Serial.println("cmd(n,'Normal Run mode (valve enabled, valve pos 0, auto_modes on)').");
	Serial.println("cmd(p,[1],[0,1,2,3,4],'set valve to position N, auto_valve off').");
	Serial.println("cmd(r,'Restore settings from EEPROM').");
	Serial.println("cmd(s,'Save settings in EEPROM').");
	Serial.println("cmd(t,[371],'Set target temperature in tenth degrees:37.1C').");
	Serial.println("cmd(t,'Get temperature').");
}

void mixer(byte v)
{
	if (v == 0)
		analogWrite(MIXER,0);
	else 
	    for(int i=3; i<11; i++) {
		analogWrite(MIXER, i*mixerspeed/10);
		if (auto_valve) valve.checkValve();
		delay(400);
 	    }
}

boolean lagoon_command(char c1, char c2, int value)
{
char reply[80];
byte d;
int tmp;
     reply[0] = 0;  
	switch(c2)
	{
		case '1': d = 1; break;
		case '0': d = 0; break;
		default : break;
	}
	switch(c1)
	{
		case 'a':
			if (d == 1) {
				auto_temp = true;
				auto_valve = true;
				auto_mixer = true;
			} else {
				auto_temp = false;
				auto_valve = false;
				auto_mixer = false;
			}
			break;
		case 'c':
		        calibration = c2 - '0';
			auto_temp = false;
			auto_valve = false;
			valve.enable(1);
			valve.position(0);
			valve.calibrate(calibration);
			break;
		case 'd':
		        valve.setAngle(c2,value);
			break;
		case 'e':
			valve.enable(d);
			break;
		case 'h':
			digitalWrite(HEATER, d);
			break;
		case 'i':
			if (c2 != 0)
				id = c2;
			else {
				Serial.print("id(");
				Serial.print(id);
				Serial.println(").");
			}
			break;
		case 'l':
	                if (d == 1)
				digitalWrite(LED, 1);
			else
				digitalWrite(LED, 0);
			break;
		case 'm':
			Serial.print("mixer(");
			Serial.print(d);
			Serial.println(").");
			Serial.println(d);
			mixer(d);
			break;
		case 'n':
		        calibration = 0;
			valve.calibrate(0);
			valve.enable(1);
			valve.position(0);
			auto_temp = true;  // Maintain Temperature Control
			auto_valve = true;  // Maintain Flow
			break;
		case 'o':
		        if (c2 == 'f') valve.setOutflowms(value);
			else           return false;
			break;
		case 'p':
		        if (c2 == 'h') printHelp();
			else {
			     auto_valve = false;
			     valve.position(c2-'0');
			}
			break;
		case 'r':  
			switch(c2) {
				case 'v': valve.report(reply);
				     	  break;
				case 't':
					Serial.print("temperature(");
					Serial.print(temp.celcius());
					Serial.println(").");
					break;
				default: strcpy(reply,saveRestore(RESTORE));
					 break;
			}
			break;
		case 's':
			strcpy(reply,saveRestore(SAVE));
			break;
		case 't': // set target(ts), get target(tt) or get current temp (t)
		        if (c2 == 's')
			   target_temperature = ((float)value)/10.0;
		        else {
			     if (c2 == 't') {
			     	    tmp = (int) (target_temperature*10.0);
				    Serial.print("target_");
			     }
			     else
				tmp = (int) (temp.celcius()*10.0);
			     Serial.print("temperature(");
			     Serial.print(tmp);
			     Serial.println(").");
			}
			break;
		case 'v':
			valve.setup_valve(c2-'0', value);
			break;
		default:
			return false;
	}
	if (strlen(reply) > 0) Serial.println(reply);
	Serial.println("end_of_data.");	
	return true;
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
			value = atoi(&is[2]);
		if (!lagoon_command(is[0], is[1], value))
			Serial.println("bad_command('" + is + "').\nend_of_data");
	}
}

/*
 * average() throw out two extreme values and average the rest
 */

float average(int *arr, int size)
{
	float avg = 0;
	int mx = 0;
        int mn = 2000;
	for (int i=0; i<size; i++) 
	{
		avg += arr[i];
		if (arr[i] < mn) mn = arr[i];
		if (arr[i] > mx) mx = arr[i];
	}
	return ( ( avg - (mn+mx) )/(size-2));
}

float stdev(int *arr, int size, float avg)
{
	float sumsq = 0;
	int mx = 0;
        int mn = 2000;
	for (int i=0; i<size; i++)
	{
		sumsq += (avg - arr[i])*(avg - arr[i]);
		if (arr[i] < mn) mn = arr[i];
		if (arr[i] > mx) mx = arr[i];
	}
	sumsq = sumsq - (avg - mn)*(avg - mn);
	sumsq = sumsq - (avg - mx)*(avg - mx);
	return sqrt(sumsq/(size-2));
}

/*
 * setup()	1) Initializes serial link
 *		2) Restores settings from EEPROM
 *		2) Calls flow_setup (pumps)
 *		3) Calls turbid_setup (LED/Optics)
 */

boolean once;

void setup()
{
	pinMode(VALVEENABLE,OUTPUT); digitalWrite(VALVEENABLE,1); //no power to valve
	pinMode(LAGOONOUT, OUTPUT);  digitalWrite(LAGOONOUT, 0);
	pinMode(HEATER,    OUTPUT);  digitalWrite(HEATER, 0);
	pinMode(LED,       OUTPUT);  digitalWrite(LED, 1);

	Serial.begin(9600); // 9600, 8-bits, no parity, one stop bit
	myservo.attach(VALVEPIN);
	valve.position(0);
        analogWrite(MIXER, 0 );     // Mixer off
	interval = millis();

//	if (true)
	if (EEPROM.read(0)==0 || EEPROM.read(0)==255)	// First time
	{
		id = '2';	// Default Lagoon ID 
		target_temperature = 34.5;
		mixerspeed = MIXERSPEED;
		valve.setOutflowms(15000);
		valve.setOutflowPin(LAGOONOUT);
		valve.setAngle('0',0);
		valve.setAngle('1',45);
		valve.setup_valve(1, 8000);
		valve.setAngle('2',90);
		valve.setup_valve(2, 6000);
		valve.setAngle('3',135);
		valve.setup_valve(3, 4000);
		valve.setAngle('4',180);
		valve.setup_valve(4, 2000);
		saveRestore(SAVE);
	}
	else
	{
		saveRestore(RESTORE); // Valve timing copied to valve object
	}
	once = true;
	auto_temp = true;  // Maintain Temperature Control
	auto_valve = true;  // Maintain Flow
	auto_mixer = true;  // Cycle magnetic mixer to avoid stalled stir-bar

calibration = 0;    // In/Out flows normal (calibration off)
	valve.calibrate(calibration);
}

int cnt_light = 0;
int cnt_mixer = 0;
void loop()
{
int t;
	respondToRequest();     // Check for command
	delay(100);
	if (auto_temp)		// Check and update heater(s)
		checkTemperature();
	if (auto_valve) {
		valve.checkValve();
	}

       // Check valve timing regularly during "longish" mixer spin down/up

       if (auto_mixer && (cnt_mixer++ % 5000 == 0)) {
	   mixer(0);
	   for (t=0;t<5;t++) {
	   	if (auto_valve) valve.checkValve();
		delay(400);
	   }
	   mixer(1);
	}
}

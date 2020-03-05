#include "param.h"
#if (ARDUINO >= 100)
 #include "Arduino.h"
#else
 #include "WProgram.h"
#endif
// All Innatrix Arduino control programs contain at least:
//
//   void respondToRequest(void)
//   bool process_command(char c1, char c2, int value)
//   void setup()
//   void loop()
//
//  If DOWNLOADING this program fails:
//
//  1) Disconnect Bluetooth (it competes with USB cable for RX/TX)
//  2) Disconnect RESET wire (without Bluetooth module, Reset held low)
//  3) Check Tools->Processor for Bootloader compatibility

#include <Wire.h>
#include <Adafruit_Sensor.h>   // Arduino: Manage Libraries
#include <Adafruit_TSL2591.h>

Adafruit_TSL2591 tsl;
int gain_setting;   // Luminometer Gain
int timing_setting; // Integration Time
boolean luxOn;

tsl2591Gain_t
tsl_gain[4] = { TSL2591_GAIN_LOW,   //    1X
	        TSL2591_GAIN_MED,   //   25X
		TSL2591_GAIN_HIGH,  //  428X
		TSL2591_GAIN_MAX }; // 9876X

tsl2591IntegrationTime_t
tsl_timing[6] = { TSL2591_INTEGRATIONTIME_100MS,
	          TSL2591_INTEGRATIONTIME_200MS,
		  TSL2591_INTEGRATIONTIME_300MS,
		  TSL2591_INTEGRATIONTIME_400MS,
		  TSL2591_INTEGRATIONTIME_500MS,
		  TSL2591_INTEGRATIONTIME_600MS };

void lux_set_timing(int t)
{
  if (t < 0 || t > 5)
     printTermInt("e",101);
  else if (luxOn)
  {
	tsl.setTiming(tsl_timing[t]);
	timing_setting = t;
  }
  else 
       printTermInt("e",99);
}

void lux_set_gain(int g)
{
   if (g < 0 || g > 3 )
      printTermInt("e",100);
   else if (luxOn) {
   	tsl.setGain(tsl_gain[g]);
	gain_setting = g;
   } else
     printTermInt("e",98);
}

void lux_init(int id)
{
  tsl  = Adafruit_TSL2591(id); // pass in id (for you later)
  if (tsl.begin())
  {
        luxOn = true;
	tsl.setGain(tsl_gain[gain_setting]);
	tsl.setTiming(tsl_timing[timing_setting]);
  }
}

int luxGain(void) // 0-3
{
  int i;
  tsl2591Gain_t gainval = tsl.getGain();
  for(i=0;i<4;i++)
    if (gainval == tsl_gain[i]) return i;
  return -1;
}

int luxTiming(void) // mSeconds
{
  return((tsl.getTiming() + 1)*100);
}

void lux_details()
{
    sensor_t sensor;
    tsl.getSensor(&sensor);
    Serial.print  ("Sensor:    "); Serial.println(sensor.name);
    Serial.print  ("Driver Ver:"); Serial.println(sensor.version);
    Serial.print  ("Unique ID: "); Serial.println(sensor.sensor_id);
    Serial.print  ("Max Value: "); Serial.print(sensor.max_value);
    Serial.println(" lux");
    Serial.print  ("Min Value: "); Serial.print(sensor.min_value);
    Serial.println(" lux");
    Serial.print  ("Resolution:"); Serial.print(sensor.resolution);
    Serial.println(" lux");  
    Serial.println("------------------------------------");
}

uint16_t luxRaw(void)
{
	if (luxOn)
	   return(tsl.getLuminosity(TSL2591_FULLSPECTRUM));
	else
	   return(0);
  //    return(tsl.getLuminosity(TSL2591_VISIBLE));
  //    return(tsl.getLuminosity(TSL2591_INFRARED));
}

boolean auto_temp;   // Automatically control Heater
boolean auto_valve;  // Automatically control Valves
boolean auto_mixer;  // Automatically control Mixer
boolean auto_air;    // Automatically control Aeration
boolean mixer_state;
 
char reply[40];

/* EEPROM SAVE AND RESTORE OF ID AND CALIBRATION CONSTANTS */

#include "EEPROM.h"
int eeSize;
int RomAddress  = 0;

byte id = 'm'; // module: generic, by default
int target_temperature;
int target_turbidity;
int target_flowrate;
int gtscale;
int gtoffset;  // Offset and scale for Turbidity calculation
int OD;
int interval;   // Variable to keep track of the time

int led600int = 200;
int laserint  = 200;
int meniscusint = 200;

int mixerspeed;
int reading[10];
boolean dflag = false;
int drate;

int cnt_light = 0;
int cnt_mixer = 0;
int looponce = 1;

// INTERRUPT ROUTINE FOR DROP COUNTING

#define pin2ISR(p)  (p==2?0:(p==3?1:(p>=18&&p<=21?23-p:-1)))
#include "drip.h"

// LEAK DETECTION

#define LEAK	(leakage()<300)

int leakage(void)
{
int leakage = analogRead(ANALOG_LEAK);
    leakage += analogRead(ANALOG_LEAK);
    leakage += analogRead(ANALOG_LEAK);
    return leakage/3;
}

// SERVO VALVE

#include <Servo.h>
Servo myservo;

/*
 * Servo write has to be in main routine and the
 * object library has to be included here.
 * This is Arduino compiler crap. Clearly, servo
 * control should be inside valve.h (try it, I dare you).
 */

void swrite(int val) {
     if (dflag) {
     	Serial.print("servo("),Serial.print(val);Serial.println(").");
     }
     myservo.write(val);
}

int valveCycleTime = DEFAULT_CYCLETIME; // constants in param.h

#include "valve.h"   // MULTI-POSITION PINCH VALVE  (inflow)

VALVE    valve = VALVE(NUM_VALVES+1, VALVEPIN); // 5-position valve on pin 8

#include "valves.h" // SOLENOID VALVES    (outflow, generally)

VALVES valves = VALVES(NUM_VALVES);

#ifndef INTERNAL // Mega has two references, but no default
#define INTERNAL INTERNAL2V56
#endif
// PROGMEM rather than F() because it is global but it requires
// a cast to ( __FlashStringHelper * ) for println()  see below

const char iface[] PROGMEM = {
   "i( module, ebutton,\n\
      [ ro( t, int:=369,  \"Temperature\"),\n\
	rw(tt, int:=350,  \"Target Temperature\"),\n\
	ro( f, int:= 0,   \"Fluorescence\"),\n\
	ro( w, int:= 500, \"Leak\"),\n\
        rw(fg, int:=2,    \"Fluorescence Gain\"),\n\
        rw(ft, int:=2,    \"Fluorescence Time\"),\n\
        rw( l, int:=100,  \"Meniscus LED\"),\n\
        rw(ms, int:=100,  \"Mixer Motor Speed\"),\n\
        rw(uv, int:=100,  \"Ultra-violet LED\"),\n\
        rw(ms, int:=100,  \"Mixer Motor Speed\"),\n\
        rw(v1, int:=1000, \"Input Valve 1 Timing\"),\n\
        rw(v2, int:=1000, \"Input Valve 2 Timing\"),\n\
        rw(v3, int:=1000, \"Input Valve 3 Timing\"),\n\
        rw(v4, int:=1000, \"Lagoon Valve 4 Timing\")\n\
        rw(wv, int:=1000, \"Waste Valve Timing\"),\n\
      ])." };

#define INTERFACE (__FlashStringHelper*)iface
      

#define EOT "end_of_data."

/*
 * PIR / I2C Temperature Stuff
 */
void initializeT()
{
  static boolean once = true;
  if (once) { Wire.begin(); once = false; }
}

#define MLX90614_I2CADDR 0x5A
#define MLX90614_TOBJ1   0x07

uint16_t objTC(void) {
  int tenths;
  uint16_t ret;
  initializeT();
  Wire.beginTransmission((uint8_t) MLX90614_I2CADDR); // start transmission
  Wire.write((uint8_t)MLX90614_TOBJ1);               // send register address for read
  Wire.endTransmission(false);              // end transmission
  Wire.requestFrom((uint8_t) MLX90614_I2CADDR, (uint8_t)3);// send data n-bytes read
  ret = Wire.read();
  ret |= Wire.read() << 8;                       // read three bytes
  uint8_t pec = Wire.read();
  tenths = ((ret<<1)-27315) / 10;  // Tenths of degrees C
//  Serial.print(ret); Serial.print("  "); Serial.println(tenths);
  return tenths;
}

int get_temperature()  // Temperature in tenths of degrees C
{
int tries = 4;
int resets = 2;
int tmp;

      while(tries-- > 0)
      {
	    tmp = objTC();
	    if (tmp>0  && tmp<800) return tmp;
	    delayMicroseconds(5000);
	    if ( tries == 0 && resets-- > 0 )
	    {
	       Wire.begin(); delayMicroseconds(5000); tries=4;
	    }
      }
      return 888;
}

/*
 * Host controller
 *
 * 1) Create nutrient valve controller
 * 2) Accept commands from main computer to:
 *     a) adjust timing
 *     b) control meniscus light
 *     c) set auto/manual temperature control

 * 3) Check temperature and manage heaters
 */

void printTermInt(const char *f,int a)
{
  sprintf(reply, "%s(%d).",f,a);
  soutln(reply);
}

void printTermUInt(const char *f,uint16_t a)
{
  sprintf(reply, "%s(%u).",f,a);
  soutln(reply);
}

void printTerm2Int(const char *f,int a,int b)
{
  sprintf(reply, "%s(%d,%d).",f,a,b);
  soutln(reply);
}

void printTerm3Int(const char *f,int a,int b,int c)
{
  sprintf(reply, "%s(%d,%d,%d).",f,a,b,c);
  soutln(reply);
}
	
void printTermFloat(char *f,double a)
{ Serial.print(f);Serial.print("(");Serial.print(a);Serial.println(")."); }

void sout(const char *str) { Serial.print(str); }
void soutln(const char *str) {	Serial.println(str); }

/* Commands:
 *  l0 :    Meniscus light off
 *  l1 :    Meniscus light on
 *  h0 :    Heater off
 *  h1 :    Heater on
 *  m0 :    Mixing motor off
 *  m1 :    Mixing motor on
 *  a0 :    Auto modes off
 *  a1 :    Auto modes on
 *  oN :    Open valve N
 *  cN :    Close valve N  (also, auto_valve mode turned ON)
 *  pN :    Prime (open valve N, auto_valve mode OFF)
 *  vNssss:    Set valve N open time to ssss ms
 *  dN :    Subtract 10ms to open time for valve N
 *
 *  {id}N : Increase/decrease open time of valve by internal increment
 *
 *  cN : Calibrate valve N
 *      Open it on schedule with other valves closed
 *  r  : Run mode (calibration off)
 */


/* Commands:
 *  a0 :    Auto modes off
 *  a1 :    Auto modes on
 *  dNaa :  Set angle for valve position N
 *   EXAMPLE  Set angle for 0th valve position at 10 degrees (rather than 0)    "d010"
 *   EXAMPLE  Set angle for 4th valve position at 178 degrees (rather than 180) "d4178"
 *  e0 :    Disable auto modes and input flow
 *  e1 :    Enable auto modes and input valve schedule
 *  h  :    Print help
 *  h0 :    Heater off
 *  h1 :    Heater on
 *  l0 :    Meniscus light off
 *  l1 :    Meniscus light on
 *  m0 :    Mixing motor off
 *  m1 :    Mixing motor on
 *  pN :    Go to Valve position N, auto_valve mode off
 *  rNM :   Set cycleModulo (Range) for valve V to M (< 256 - it is a byte)
 *  r  :    Restore settings from EEPROM
 *  s  :    Save settings in EEPROM
 *  tNNN:   Set target temperature in tenths of degrees  371 = 37.1C
 *  t  :    Get current temperature
 *
 *  n  : Normal (run mode) : calibration off, auto modes on
 */

void printHelp(void)
{
int i;
byte *bp = valve.getAngles();
byte *rp = valve.getRanges();
int  *ip = valve.getTimes();          // PINCH
uint16_t  *times = valves.getTimes(); // SOLENOID

	Serial.println(F("a([0,1],'auto modes off/on')."));
	Serial.print(F("b(["));Serial.print(turbidity());
	Serial.println(F("],'Get turbidity')."));
	Serial.print(F("tb(["));Serial.print(target_turbidity);
	Serial.println(F("],'Get/Set target turbidity')."));
	Serial.print(F("cy([")); Serial.print(valveCycleTime);
	Serial.println(F("],'Valve cycle time')."));
	Serial.print(F("d([0,1,2,3,4],["));
	 for(i=0;i<5;i++) { Serial.print(*bp++); if (i != 4) Serial.print(","); }
        Serial.println("],'Valve Pos = 0-180 Angle').");
	Serial.println(F("e([0,1],'enable inputs vs. calibration')."));
	Serial.println(F("f([0,1,z],'get drip count(s) or zero counters')."));

	Serial.println(F("h([0,1],'heater off/on auto_temp off')."));
	Serial.println(F("l([0,1],'light off/on')."));
	Serial.println(F("m([0,1],'mixer off/on')."));
	Serial.print(F("ms(["));
	Serial.print(mixerspeed);
	Serial.println(F("],'get/set mixer speed')."));
	Serial.println(F("n('Normal mode (valve enabled, pos 0, auto on)')."));
	Serial.println("p([1],[0,1,2,3,4],'Valve to position N, auto off').");
	
	Serial.print(F("r([1,2,3,4],["));
	 for(i=1;i<NUM_VALVES+1;i++) {
	   Serial.print(*rp++); if(i<NUM_VALVES) Serial.print(","); 
	   }
        Serial.println(F("],'Valve Cycle Modulo')."));
		
	Serial.println(F("r('Restore settings from EEPROM')."));
	Serial.println(F("s('Save settings in EEPROM')."));
	Serial.print(F("t(["));Serial.print(get_temperature());
	Serial.println(F("],'Get temperature in tenth degrees C')."));
	Serial.print(F("tt([")); Serial.print(target_temperature);
	Serial.println(F("],'Get/Set target temp in tenth degrees C')."));
	Serial.print(F("tf([")); Serial.print(target_flowrate);
	Serial.println(F("],'Get/Set target flow rate ml/hr')."));
	Serial.print(F("v([1,2,3,4,5],["));
	 for(i=1;i<NUM_VALVES+1;i++) {
	   Serial.print(*ip++); Serial.print(","); }
	Serial.print(valves.getTime(0));
        Serial.println(F("],'valve open mS')."));
	Serial.println(F("z('Zero EEPROM')."));
}

// PID Controller to keep temperature within 0.5 degree C

#include "pid.h"         // Local copy
double Setpoint, TempInput, TempOutput;
double Kp=2, Ki=5, Kd = 1;
int windowSize = 10000;
unsigned long windowStartTime;

PID temppid(&TempInput, &TempOutput, &Setpoint, Kp, Ki, Kd, DIRECT);

#if 0
void checkTemperature()
{
int t = get_temperature();
	if (t < target_temperature)      digitalWrite(HEATER,0);
	if (t > target_temperature + 5)  digitalWrite(HEATER,1);
}
#else
void checkTemperature()
{
unsigned long int dtee;
int t   = objTC();
  TempInput = (float) t;
//  Serial.println("TempInput");
//  Serial.println(TempInput);
  temppid.Compute();
  if (dflag && ((drate++ % 500) == 0 )) {
     Serial.print(target_temperature);
     Serial.print(", ");
     Serial.print(TempInput);
     Serial.print(", ");
     Serial.println(TempOutput);
  }
  if (millis() - windowStartTime > windowSize) // move Relay Window
  {
    windowStartTime += windowSize;
  }
  dtee = millis() - windowStartTime;
// Serial.println("delta ");
//  Serial.println(dtee);
//  Serial.println(TempOutput);
  if (TempOutput < millis() - windowStartTime) digitalWrite(HEATER, 0);
  else                                         digitalWrite(HEATER, 1);
}
#endif

// 'RomAddress' global is bumped by successive
// calls to moveData( SAVE|RESTORE, size, ptr)

void moveData(int op, int size, byte *loc)
{
	for(int i=size;i>0;i--)
		if (op == SAVE)
			EEPROM.write(RomAddress++,*loc++);
		else
			*loc++ = EEPROM.read(RomAddress++);
}

const char *saveRestore(int op)
{
	RomAddress = 0;
	moveData(op, 1, &id);
	moveData(op, sizeof(int), (byte *) &target_temperature);

	// SOLENOIDS
	moveData(op, NUM_VALVES*sizeof(uint16_t), (byte *) valves.getTimes());

        // PINCH VALVE (SERVO)
	moveData(op, NUM_VALVES*sizeof(int),     valve.getTimesbp());
	moveData(op, (NUM_VALVES+1)*sizeof(byte),valve.getAngles());
	moveData(op, (NUM_VALVES+1)*sizeof(byte),valve.getRanges());
	moveData(op, sizeof(int),   (byte *)&valveCycleTime);
	
	moveData(op, sizeof(int), (byte *) &target_turbidity);
	moveData(op, sizeof(int), (byte *) &gtscale);
	moveData(op, sizeof(int), (byte *) &gtoffset);
	moveData(op, sizeof(int), (byte *) &mixerspeed);
	moveData(op, sizeof(int), (byte *) &gain_setting);
	moveData(op, sizeof(int), (byte *) &timing_setting);
	moveData(op, sizeof(int), (byte *) &target_flowrate);
	if (op == SAVE) return("save.");
	else            return("restore.");
}

// TURBIDITY

int turbread[10];
int turbindex = 0;

int ODhist[10];
int ODhindex = 0;

#define TURB_DELAY 2
int turbdelay = 0;

int turbidity() {
//	return analogRead(ANALOG_TURBIDITY);
	return OD;
}

/* Assumes constant offset for turbidity calculation */
/* number in thousandths. e.g. 400 = (OD600 0.400) */

void forceTurbidity(int currentTurbidity)
{
   int i;
   unsigned int reading = 0;
   for (i=0; i<5; i++) reading += analogRead(ANALOG_TURBIDITY);
   reading = reading/5;
   int calcturb = reading/gtscale + gtoffset;
   sprintf(reply, "read_turbidity(%d,%d).", reading, calcturb);
   int newscale = reading/(currentTurbidity - gtoffset);
//   sprintf(reply, "scdelta(%d,%d).", gtscale, newscale);
   gtscale = newscale;
//   saveRestore(SAVE);
   soutln(reply);
}

int checkTurbidity() {
int highlow = 0;
int i, t, avg;
// Read Turbidity and bump the ReadArray index
	turbread[turbindex] = analogRead(ANALOG_TURBIDITY);
	turbindex = (turbindex+1)%10;

// Average the last ten values and bump the delay index
	avg = 0;
	for (i=0;i<10;i++) avg += turbread[i];
	OD = avg/10;
	turbdelay = (turbdelay+1)%TURB_DELAY;
// After a certain delay, store the average Optical Density
	if (turbdelay == 0) {
		ODhist[ODhindex] = OD;
		ODhindex = (ODhindex+1)%10;
	}
// Unanimous vote of last ten delayed averages up or down
	t = 0;
	for(i=0;i<10;i++) {
		if (ODhist[i] > target_turbidity) t++;
		if (ODhist[i] < target_turbidity) t--;
	}
	// High or Low Turbidity must be unanimous
	return (t/10);  // -1, 0, +1 
}

// Make sure to check on things while motor is spinning up
// Valve timings are precise and user will time-out if we don't respond.
#if 0
void mixer(byte v)
{
	if (v == 0) {
		analogWrite(MIXER,0);
		mixer_state = false;
	} else {
	    if (!mixer_state) {
	    	for(int i=3; i<13; i++) {
			analogWrite(MIXER, i*mixerspeed/10);
			respondToRequest();
			if (auto_valve) valves.checkValves();
			for(int j=0;j<100;j++)
				delayMicroseconds(4000);
		}
		analogWrite(MIXER, mixerspeed);
		mixer_state = true;
	    }
	}
}
#else
void mixer(byte v)
{
	if (v == 0) {
		analogWrite(MIXER,0);
		auto_mixer = false;
	} else {
	    for(int i=3; i<15; i++) {
		analogWrite(MIXER, i*mixerspeed/13);
		if (auto_valve) valve.checkValve();
		delay(200);
 	    }
	    analogWrite(MIXER, mixerspeed-(mixerspeed/10));
	    auto_mixer = true;
	}
}
#endif

void respondToRequest(void)
{
	String is = "";
	while (Serial.available() > 0)  // Read a line of input
	{
		int c  = Serial.read();
		if ( c < 33 ) break;
		is += (char)c;
		if (Serial.available() == 0) // It is possible we're too fast
			delayMicroseconds(10000);
	}
	if ( is.length() > 0 )  {   // process the command
		int value = 0;
		if (is.length() > 2) {
			value = atoi(&is[2]);
			if (value == 0) value = 1;
		}
		if (!process_command(is[0], is[1], value)) {
			Serial.println("er(" + is + ").");
			Serial.println(EOT);
		}
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
void valveDefaults(int vnum, int Angle, int Time, int Modulo)
{
	valve.setAngle(vnum,     Angle);
	valve.setup_valve(vnum,   Time);
	valve.setRange(vnum,    Modulo);
}

/*
 * setup()	1) Initialize serial link
 *		2) Restores settings from EEPROM
 *		2) Calls flow_setup (pumps)
 *		3) Calls turbid_setup (LED/Optics)
 */
boolean once;

void setup()
{
int i;
	Serial.begin(9600); // 9600, 8-bits, no parity, one stop bit
	auto_temp = true;  // Maintain Temperature Control
	auto_valve = true; // Maintain Flow (check turbidity)
	auto_mixer = true; // Maintain Mixer
	auto_air = true;   // Maintain Aeration
	dflag = false;
	drate = 0;
	pinMode(HEATER,    OUTPUT);  digitalWrite(HEATER, 1);
	pinMode(UVC,       OUTPUT);  digitalWrite(UVC, 1);
	pinMode(DRIP,      INPUT);
	pinMode(DRIP2,     INPUT);

	pinMode(VALVEDISABLE,OUTPUT);
	//  Active Low (power to valve) default 1 == no power
	digitalWrite(VALVEDISABLE,0);   //  Enable servo power
	delay(300);
	myservo.attach(VALVEPIN);
	valve.position(0);              //  move to position 0
	delay(500);
	digitalWrite(VALVEDISABLE,1);  	//  and then disable
	pinMode(MIXER,OUTPUT);
	pinMode(LASER,OUTPUT);
	digitalWrite(LASER,1);
	pinMode(MENISCUS,OUTPUT);

        analogWrite(MIXER, 0 );     // Mixer off
	interval = millis();

	pinMode(WASTE,  OUTPUT);  // Solenoid Valve for Drain
	digitalWrite(WASTE,   0);
	valves.setup_valve(0, WASTE, 300, OUTFLOW); // SOLENOID SETUP

	pinMode(HEATER, OUTPUT); digitalWrite(HEATER, 0);
	pinMode(AIR, OUTPUT); digitalWrite(AIR, 0);
	
	analogWrite(MENISCUS, meniscusint);
        analogWrite(LED600,led600int);
	delayMicroseconds(10000);
	analogWrite(MIXER, 0);   // Mixer motor off

	interval = millis();
	eeSize = sizeof(float) + (NUM_VALVES+2)*sizeof(int) + (NUM_VALVES+2)*sizeof(byte);

	if (EEPROM.read(0)==0 || EEPROM.read(0)==255)	// First time
	{
		id = 'm';	// Initially Module (assign 'l' or 'h')
		target_temperature = 370;
		target_turbidity = 400;
		target_flowrate = 30;
		gtscale = 9100;
		gtoffset = 0.0;
		mixerspeed = MIXERSPEED;
		gain_setting = 2;
		timing_setting = 3;
		valveDefaults(0,  0, 1000, 1);
		valveDefaults(1, 45, 1000, 1);
		valveDefaults(2, 90, 1000, 1);
		valveDefaults(3,139, 1000, 4);
		valveDefaults(4,180, 1000, 4);
		valveCycleTime = DEFAULT_CYCLETIME;
		valves.setTime(0,300); // Solenoid valve 0 -> 300mS
		saveRestore(SAVE);
	}
	else
	{
		saveRestore(RESTORE); // Valve timing copied to valve object
	}
	luxOn = false;
        lux_init(2591);
	// Propagate valve cycle time (two valve systems) also after "restore"
	valve.setCycleTime(valveCycleTime);  // Pinch valves 1-4
        valves.setCycleTime(valveCycleTime); // Solenoid valves 5...->(0...)
	once = true;
	for (i=0;i<10;i++) // Fill OD600 averaging vector
	    turbread[i] = analogRead(ANALOG_TURBIDITY);
	mixer_state = false;
	mixer(1); // Turn on Mixer

        windowStartTime = millis();
	windowSize = 10000;
       	Setpoint = (double) target_temperature;
	temppid.SetOutputLimits(0, windowSize);
	temppid.SetControllerDirection(DIRECT);
	temppid.SetMode(AUTOMATIC);

	auto_temp = true;   // Maintain Temperature Control
	auto_valve = true;  // Maintain Flow
	auto_mixer = true;  // Cycle magnetic mixer to avoid stalled stir-bar
	if (dflag) Serial.println("reset.");
        attachInterrupt(pin2ISR(DRIP), dripCatcher0, RISING);
        attachInterrupt(pin2ISR(DRIP2), dripCatcher1, RISING);
}

#define valveRange(c)  ((c) >= '0' && (c) < '5')

boolean process_command(char c1, char c2, int value)
{
char reply[80];
byte d;
int tmp;
int vnum;
char vcmd[3];
  vcmd[0] = 'v';
  vcmd[1] = c2;
  vcmd[2] = 0;

     reply[0] = 0;

	switch(c2)
	{
		case '1': d = 1; break;
		case '0': d = 0; break;
		default : d = 9; break;
	}
	switch(c1)
	{
		case 'a':
		 switch(c2) {
			case 'b': led600int = value;
			          analogWrite(LED600,led600int);
				  checkTurbidity();
				  printTerm2Int("ab",led600int,turbidity());
				  break;
			case 'l': laserint = value;
			          digitalWrite(LASER,d);
				  printTermInt("al",d);
				  break;
			case 'm': meniscusint = value;
			          analogWrite(MENISCUS,meniscusint);
				  printTermInt("am",meniscusint);
				  break;
			case '1':
				auto_temp = true;
				auto_valve = true;
				auto_mixer = true;
				auto_air = true;
				break;
			case '0':
				auto_temp = false;
				auto_valve = false;
				auto_mixer = false;
				auto_air = false;
				break;
			}
			break;
		case 'b':
			printTermInt("b",turbidity());
			break;
		case 'c':
			if ( c2 == 'y' ) {
			   if (value != 0) {
			      valveCycleTime = value;
			      valve.setCycleTime(value);
			      valves.setCycleTime(value);
			   } else
			     printTerm3Int("cycleTime",
			     valveCycleTime,
			     valve.getCycleTime(),
			     valves.getCycleTime());
			}
			else {
			     vnum = (int)(c2 - '0');
			     if (valveRange(c2)) {
			     	printTermInt("e",(int)c2);
			     } else if (vnum == 5) {
			       valves.closeValve(0);
			       auto_valve = true;
			     }
			}
			break;
		case 'd':
		     if (valveRange(c2)) {
		         vnum = (int)(c2 - '0');
			 if (value == 0)
			     printTermInt(vcmd,valve.getAngle(vnum));
			 else
			     valve.setAngle(vnum,value);
		     } else
		         printTermInt("e",(int)c2);
		     break;
		case 'e':
		        break;  // Enable? NYI
			if (d == 1) {
				valve.enable(true);
				auto_temp = true;
				auto_valve = true;
			} else {
			        valve.enable(false);
				auto_temp = false;
				auto_valve = false;
			}
			break;
		case 'f':
			switch(c2) 
			{
			case '0':
			     printTermUInt("f0",cflowRate(0));
			     break;
			case '1':
			     printTermUInt("f1",cflowRate(1));
			     break;
			case 'c':
			     clear_flow_count(0);
			     clear_flow_count(1);
			     break;
			case 'g':
			     lux_set_gain(value);
			     break;
			case 'r':
			     printTermUInt("fr",flowRate(value));
			     break;
			case 't':
			     lux_set_timing(value);
			     break;
			case 'x':
			     printTermUInt("f0",cflowRate(0));
			     printTermUInt("f1",cflowRate(1));
			     reportDRIP();
			     break;
			case 'z':
			     clear_flow_count(0);
			     clear_flow_count(1);
			     break;
			default:
			     printTermUInt("f",luxRaw());
			}
			break;
		case 'h':
			switch(c2)
			{
			 case '0': digitalWrite(HEATER,0); break;
			 case '1': digitalWrite(HEATER,1); break;
			 case 'i':
			   sprintf(reply,"odHistory(%d,%d,%d,%d,%d,%d,%d).",
			      ODhist[0],ODhist[1],ODhist[2],ODhist[6],ODhist[7],ODhist[8],ODhist[9]);
			      soutln(reply);
			      break;
			 case 'v':
			      valves.report(reply);
			      soutln(reply);
			      break;
			 default :
			 	 printHelp();
			}
			break;
		case 'i':
			if (c2 == 'd') {
			   if (value==0)
			      printTermInt("id",id);
			   else
			      id = (byte)value;
			} else
			  Serial.println(INTERFACE);
			break;
		case 'l':
		       switch(d) {
		        case 0:
			    digitalWrite(UVC, 0);
			    digitalWrite(LASER, 0);
		       	    analogWrite(MENISCUS, 0);
			    analogWrite(LED600,   0);
			    break;
			case 1:
		       	    analogWrite(MENISCUS, meniscusint);
			    analogWrite(LED600,   led600int);
			    digitalWrite(LASER, 1);
			    digitalWrite(UVC, 1);
			    break;
			default:
			  printTermInt("led600",meniscusint);
			}
		case 'm':
		     if (c2 == 's') {
			if (value == 0)
				printTermInt("ms",mixerspeed);
			else {
				mixerspeed = value;
				analogWrite(MIXER, mixerspeed);
			}
		     }
		     else if (d == 9)
		     	  printTermInt("m",auto_mixer);
		     else
			mixer(d);
		     break;
		case 'n':
		     if (value == 0) {
		         auto_temp = true;
			 auto_valve = true;
			 auto_mixer = true;
                     } else forceTurbidity(value);
		     break;
		case 'o':
			switch(c2)
			{
			 case '1':
			 case '2':
				digitalWrite(AIR,1);
				break;
			 case '-':
			 case '0':
				digitalWrite(AIR,0);
				break;
			default: 
		     	  printTerm2Int("o2",auto_air,digitalRead(AIR));
			}
			break;
		case 'p': // Direct Pinch Valve Position
		        if (valveRange(c2)) {
			  auto_valve = false;
			  valve.position(c2-'0');
			} else 
			   printTermInt("e", (int)c2);
			break;
		case 'r':
		        vnum = (int)(c2 - '0');
		        if (valveRange(c2)) {
			   if (value == 0)
			     printTermInt(vcmd,valve.getRange(vnum));
			   else
			     valve.setRange(vnum, value);
			} else
		        switch(c2) {
			 case 'v':
			      valve.report(reply);
			      break;
			 case '5':
			   printTermInt("e", (int)c2);
			   break;
			 case 0:
			   saveRestore(RESTORE);	
			   valve.setCycleTime(valveCycleTime);  // Pinch
			   valves.setCycleTime(valveCycleTime); // Solenoid
			   break;
			 default:
			   printTermInt("e", 200);
			}
			break;
		case 's':
		        if (c2 == 'c') {
			   valveCycleTime = value;
			   valve.setCycleTime(value);
			   valves.setCycleTime(value);
			} else saveRestore(SAVE);
		        break;
		case 't':
		        switch(c2) {
			  case 't':
			  if (value == 0)
			     printTermInt("tt",target_temperature);
			  else
			     target_temperature = value;
			   break;
			  case 'b':
			  if (value == 0) {
			   printTermInt("tb",target_turbidity);
			   } else target_turbidity = value;
			   break;
			  case 'f': // Target Flow Rate ml/Hour
			  if (value == 0) {
			   printTermInt("tf",target_flowrate);
			   } else target_flowrate = value;
			   break;
			 default:
			   printTermInt("t",get_temperature());
			}
			break;
		case 'u':  // d = numerical value of c2 {0,1}
		     digitalWrite(UVC, d);
		     break;
		case 'v':
		        if (valveRange(c2)) {
			    vnum = (int)(c2 - '0');
			    if (value > 0) 
			        valve.setTime(vnum,value);
			    else
			       printTermUInt(vcmd,valve.getTime(vnum));
			   break;
			} else if (c2 == '5') { // Only one extra (SOLENOID) valve for now
			    if (value > 0) 
			        valves.setTime(0,value);
			    else
			       printTermUInt(vcmd,valves.getTime(0));
			} else
			  printTermInt("e",(int)(c2-'0'));
			break;
		case 'w':
		        printTermInt("w", leakage());
			break;
		case 'z':
			int i;
			EEPROM.write(0,0); // setup() will overwrite on reset
			printTermInt("z",0);
			break;
		default:
			return false;
	}
	soutln(EOT);
	return true;
}

/* 1) Create valve controller
 * 2) Accept commands from main computer to:
 *     a) adjust timings
 *     b) control meniscus light
 *     c) set auto/manual temperature control
 *     d) set auto/manual mixer control
 *     d) set auto/manual flow control
 * 3) Check temperature and manage lagoon heater
 */

void loop()
{
int tb_thresh;
int t;
        respondToRequest();     // Check for command
	delayMicroseconds(5000);
	if (auto_temp)		// Check and update heater(s)
		checkTemperature();
	if (auto_valve)		// Check and update nutrient valve
		valves.checkValves();

	// Check valve timing regularly during pause before mixer spin up
       if (auto_mixer && (cnt_mixer++ % 5000 == 0)) {
	   mixer(0);
	   for (t=0;t<40;t++) {
	   	if (auto_valve) valve.checkValve();
		delay(50);
	   }
	   mixer(1);
	}
	else if (auto_valve) valve.checkValve();
	delayMicroseconds(5000);
	if (auto_temp)	checkTemperature();
	tb_thresh = checkTurbidity();
}



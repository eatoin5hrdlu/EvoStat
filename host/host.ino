#include "param.h"        // Includes param.h (change constants there)
#if (ARDUINO >= 100)
 #include "Arduino.h"
#else
 #include "WProgram.h"
#endif
#include <Wire.h>
#include <Adafruit_Sensor.h>
#include <Adafruit_TSL2591.h>

// void      lux_init(int id)
// uint16_t  getLux()
// Gain_t    luxGain()  [0-3]
// Timing_t  luxTiming()  [0-5])
// void      lux_details()

Adafruit_TSL2591 tsl;
int gain_setting;   // Luminometer
int timing_setting;
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

#ifndef INTERNAL // Mega has two references, but no default
#define INTERNAL INTERNAL2V56
#endif

void printInterface()
{
Serial.println(F("iface( cellstat, ebutton,\n\
      [ ro(t,  int:=372, \"Temperature\"),\n\
	rw(tt, int:=364, \"Target Temperature\"),\n\
	ro( b, int:=400, \"Turbidity\"),\n\
	rw(tb, int:=400, \"Target Turbidity\"),\n\
	ro( w, int:=500, \"Leak Detection\"),\n\
        rw(v0, int:=1000, \"Host Nutrient Supply\"),\n\
        rw(v1, int:=1000, \"Host Supply 2 Valve Timing\"),\n\
        rw(v2, int:=1000, \"Host Supply 3 Valve Timing\")\n\
      ])."));
}

#include "valves.h"        // not!Includes param.h (change constants there)
VALVES valves = VALVES(NUM_VALVES);

//#include "Adafruit_MLX90614.h"
//Adafruit_MLX90614 mlx;
//#include <MLX90614.h>
//MLX90614 mlx;

//#define DEBUG 1
#define EOT "end_of_data."
/* EEPROM SAVE AND RESTORE OF ID AND CALIBRATION CONSTANTS */


#include "EEPROM.h"
int RomAddress  = 0;

byte id = 'z'; // Zeno = unassigned, by default
int gcycletime;
int target_temperature;
int target_turbidity;
int gtscale;
int gtoffset;  // Offset and scale for Turbidity calculation
int OD;
int interval;   // Variable to keep track of the time
int mixerspeed;
int reading[10];
boolean dflag = false;
int drate;

/*
 * Temperature Stuff
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

/*
 * Host controller
 *
 * 1) Create nutrient valve controller
 * 2) Accept commands from main computer to:
 *     a) adjust timing
 *     b) control meniscus light
 *     c) set auto/manual temperature control

 * 3) Check temperature and manage host and nutrient heaters
 */

boolean auto_temp;   // Automatically control Heater
boolean auto_valve;  // Automatically control Valves
boolean auto_mixer;  // Automatically control Mixer
boolean auto_air;    // Automatically control Aeration
boolean mixer_state;
boolean bluetooth;
 
char reply[40];

#ifdef WIFI
#include "secrets.h"
#include "wifi.h"
WIFI w = WIFI();
#endif

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
	


void sout(const char *str) {
#ifdef WIFI
	w.mysend(str);
#else
	Serial.print(str);
#endif
}

void soutln(const char *str) {
#ifdef WIFI
	w.mysend(str);
#else
	Serial.println(str);
#endif
}

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

void printHelp(void)
{
int i;
int *times = valves.getTimes();

	Serial.print(
F("cmd(a,[0,1],'set auto modes off/on').\n\
cmd(cl,'clear backlog (no output)').\n\
cmd(v,[0,1,2,3,4],["));
	 for(i=0;i<NUM_VALVES;i++) {
	   Serial.print(*times++); if (i<(NUM_VALVES-1)) Serial.print(","); }
        Serial.println(F("],'valve open msec V0=NUTRIENT')."));
	Serial.println(F("cmd(d,[0,1],'debug off or on')."));
	Serial.println(F("cmd(e,[0,1],'enable inputs vs. flow calibration')."));
	Serial.println(F("cmd(h,[0,1],'heater off/on auto_temp off')."));
	Serial.println(F("cmd(l,[0,1],'light off/on')."));
	Serial.println(F("cmd(m,'get mixer speed')."));
	Serial.print(F("cmd(ms,["));
	Serial.print(mixerspeed);
	Serial.println(F("],'set mixer speed')."));
	Serial.println(F("cmd(m,[0,1],'turn mixer off/on')."));
	Serial.println(F("cmd(n,'Normal Run mode (valve enabled, valve pos 0, auto_modes on)')."));
	Serial.println(F("cmd(p,[h],'printHelp -this list of commands')."));
	Serial.println("cmd(p,[0,1,2,3,4],'foce valve open').");
	Serial.println("cmd(r,'Restore settings from EEPROM').");
	Serial.println("cmd(s,'Save settings in EEPROM').");
	Serial.print("cmd(t,[");Serial.print(target_temperature);
	Serial.println("],'Get current temperature in tenth degrees C').");
	Serial.print("cmd(tt,[");Serial.print(target_temperature);
	Serial.println("],'Get/Set target temperature in tenth degrees C').");
	Serial.println("cmd(t,'Get temperature').");
	Serial.println("cmd(z,'Zero EEPROM').");
}

// All Pathe Arduino control programs contain at least:
//
//   void respondToRequest(void)
//   bool process_command(char c1, char c2, int value)
//   void setup()
//   void loop()
//

#ifdef WIFI
void wfRespondToRequest(void)
{
  char c1 = NULL, c2 = NULL;
  int value = 0;

  if (Serial.available())
  {
    if ( !w.connected() )
    {
        w.accept();
    }
    else
    {
	if (w.myrecv(&c1, &c2, &value))
	{
	    wfProcess_command(c1, c2, value);
        }
    }
  }
}

bool wfProcess_command(char c1, char c2, int value)
{
  if (c1 == 'x' && c2 == 'x' && value == -1)
  {
    w.mysend("closed(x,x,-1).");
    delayMicroseconds(10000);
    w.reboot(3); // Nothing short of a full restart will work
  }
  else
  {
    sprintf(reply, "thanks[%c][%c]%d", c1, c2, value);
    return w.mysend(reply);
  }
}
#endif

// PID Controller to keep temperature within 0.5 degree C

#include "pid.h"         // Local copy
double Setpoint, TempInput, TempOutput;
double Kp=2, Ki=5, Kd = 1;
int windowSize = 10000;
unsigned long windowStartTime;

PID temppid(&TempInput, &TempOutput, &Setpoint, Kp, Ki, Kd, DIRECT);

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

// OLD CODE
//	if (t < target_temperature - 2) digitalWrite(HEATER,1);
//	else                            digitalWrite(HEATER,0);
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

void saveRestore(int op)
{
#ifdef DEBUG
	if (op == SAVE) sout("save.");
	else            sout("restore.");
#endif
	RomAddress = 0;
	moveData(op, 1, &id);
	moveData(op, sizeof(int), (byte *) &target_temperature);
	moveData(op, NUM_VALVES*sizeof(uint16_t), (byte *) valves.getTimes());
	moveData(op, sizeof(int), (byte *) &target_turbidity);
	moveData(op, sizeof(int), (byte *) &gtscale);
	moveData(op, sizeof(int), (byte *) &gtoffset);
	moveData(op, sizeof(int), (byte *) &gcycletime);
	moveData(op, sizeof(int), (byte *) &mixerspeed);
	moveData(op, sizeof(int), (byte *) &gain_setting);
	moveData(op, sizeof(int), (byte *) &timing_setting);
}

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
        if (digitalRead(LASER) != 1) {
	   digitalWrite(LASER,1);
	   delayMicroseconds(10000);
	}
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


boolean cellstat_command(char c1, char c2, int value)
{
int vn;
byte d;
	switch(c2)
	{
		case '1': d = 1; break;
		case '0': d = 0; break;
		default : d = 9; break;
	}
	switch(c1)
	{
		case 'a':
			if (d == 1) {
				auto_temp = true;
				auto_valve = true;
				auto_mixer = true;
				auto_air = true;
			} else {
				auto_temp = false;
				auto_valve = false;
				auto_mixer = false;
				auto_air = false;
			}
			break;
		case 'b':
			printTermInt("b",turbidity());
			break;
		case 'c':
			valves.closeValve(c2);
			auto_valve = true;
			break;
		case 'd':
			if (c2 == '1') dflag = true;
			else       dflag = false;
			break;
		case 'f':
			switch(c2) 
			{
			case 'g':
			     lux_set_gain(value);
			     break;
			case 't':
			     lux_set_timing(value);
			     break;
			default:
			     printTermUInt("f",luxRaw());
			}
			break;
		case 'h':
			switch(c2)
			{
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
			  printInterface();
			break;
		case 'l':
			digitalWrite(JARLIGHT, d);
			digitalWrite(LASER,    d);
			break;
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
			forceTurbidity(value);
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
		case 'p':
			auto_valve = false;
			valves.openValve((int)c2-'0');
			break;
		case 'r':
			saveRestore(RESTORE);
			break;
		case 's':
		        switch(c2) {
			  case 'c':
			     gcycletime = value;
			     valves.setCycletime(value);
			     break;
			}
			saveRestore(SAVE);
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
			 default:
			   printTermInt("t",get_temperature());
			}
			break;
		case 'v':
		        vn = (int)(c2 - '0');
			if (vn > -1 && vn < NUM_VALVES)
			{
			  if (value > 0) 
			     valves.setTime(vn,value);
			  else {
			  char v[3];
			   v[0] = 'v';
			   v[1] = c2;
			   v[2] = 0;
			   printTermUInt(v,valves.getTime(vn));
			   }
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

void respondToRequest(void)
{
#ifdef WIFI
	wfRespondToRequest();
#else
	btRespondToRequest();
#endif
}

#ifndef WIFI
void btRespondToRequest(void)
{
	String is = "";
	while (Serial.available() > 0)  // Read a line of input
	{
		int c  = Serial.read();
		if ( c < 32 ) break;
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
		if (!cellstat_command(is[0], is[1], value)) {
			Serial.println("er(" + is + ").");
			Serial.println(EOT);
		}
	}
}
#endif

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
int i;
	Serial.begin(9600); // 9600, 8-bits, no parity, one stop bit
	bluetooth = true;
	auto_temp = true;  // Maintain Temperature Control
	auto_valve = true; // Maintain Flow (check turbidity)
	auto_mixer = true; // Maintain Mixer
	auto_air = true;   // Maintain Aeration
	dflag = false;
	drate = 0;

	pinMode(NUTRIENT, OUTPUT); digitalWrite(NUTRIENT, 0);
	pinMode(INDUCE1,  OUTPUT); digitalWrite(INDUCE1,  0);
	pinMode(INDUCE2,  OUTPUT); digitalWrite(INDUCE2,  0);
	valves.setup_valve(0,NUTRIENT,3000,INFLOW);
	valves.setup_valve(1,INDUCE1,500,INFLOW);
	valves.setup_valve(2,INDUCE2,400,INFLOW);

	pinMode(HOSTOUT,  OUTPUT);   // Not currently used (open time = 0ms)
	digitalWrite(HOSTOUT,   0);
	valves.setup_valve(1,HOSTOUT,0,OUTFLOW);

	pinMode(HEATER, OUTPUT); digitalWrite(HEATER, 0);
	pinMode(AIR, OUTPUT); digitalWrite(AIR, 0);
	pinMode(LED, OUTPUT);  digitalWrite(LED, 0);
	pinMode(JARLIGHT, OUTPUT);  digitalWrite(JARLIGHT, 1);
	pinMode(LASER, OUTPUT);  digitalWrite(LASER, 1);
	pinMode(MIXER, OUTPUT);  // Don't need pinMode for PWM output

	analogWrite(MIXER, 0);

	interval = millis();
//	mlx = Adafruit_MLX90614(); // Don't use this kludge anymore
//	mlx = MLX90614();
//	mlx.begin();   // Initialize Mexexis Thermometer
	if (EEPROM.read(0)==0 || EEPROM.read(0)==255)	// First time
	{
		id = 'h';	// Default Lagoon ID (haldane)
		target_temperature = 370;
		gcycletime = DEFAULT_CYCLETIME;
		target_turbidity = 400;
		gtscale = 9100;
		gtoffset = 0.0;
		mixerspeed = MIXERSPEED;
		gain_setting = 2;
		timing_setting = 3;
		saveRestore(SAVE);
	}
	else
	{
		saveRestore(RESTORE);
#ifdef DEBUG
		sprintf(reply,"tmtbscale(%d,%d,%d).",
			target_temperature,
			target_turbidity,gtscale);
//		soutln(reply);
#endif
	}
	luxOn = false;
        lux_init(2591);
        valves.setCycletime(gcycletime);
	once = true;
	for (i=0;i<10;i++) // Fill averaging vector
	    turbread[i] = analogRead(ANALOG_TURBIDITY);
	mixer_state = false;
	mixer(1);

        windowStartTime = millis();
	windowSize = 10000;
       	Setpoint = (double) target_temperature;
	temppid.SetOutputLimits(0, windowSize);
	temppid.SetControllerDirection(DIRECT);
	temppid.SetMode(AUTOMATIC);
}

#define LEAK	(leakage()<300)

int leakage(void)
{
int leakage = analogRead(ANALOG_LEAK);
    leakage += analogRead(ANALOG_LEAK);
    leakage += analogRead(ANALOG_LEAK);
    return leakage/3;
}

int cnt_light = 0;
int cnt_mixer = 0;
int looponce = 1;


void loop()
{
int tb_thresh;
	respondToRequest();     // Check for command
	delayMicroseconds(5000);
	if (auto_temp)		// Check and update heater(s)
		checkTemperature();
	if (auto_valve)		// Check and update nutrient valve
		valves.checkValves();
	if (auto_mixer)		// Restart the motor
		mixer(1);
	delayMicroseconds(5000);
	tb_thresh = checkTurbidity();
}


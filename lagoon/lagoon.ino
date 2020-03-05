/*
 * Lagoon controller
 */

#define pin2ISR(p)  (p==2?0:(p==3?1:(p>=18&&p<=21?23-p:-1)))

const int dripPin = 2;  // PIN with ISR for photo-interrupter

volatile int     DRIP_count;
volatile int     DRIP_events;
volatile long    DRIP_time;
int              last_DRIP_count;

void dripCatcher() // 0 -> 1 transition on pin 2
{
long now = millis();
     DRIP_events++;                 // Two events <10mS
     if ( (now - DRIP_time) > 10 )  // is the same drop
     	DRIP_count++;
     DRIP_time = now;
}


/*
 * WIRING:
 *  +5V                  Orange
 *  HEATER (active-low)  Orange-white
 *  M-LED (active-low)   Blue
 *  UV-LED (active-low)  Blue-white
 *  GND                  Green
 *  MIXER (active-high)  Green-white
 *  SCL                  Brown
 *  SDA                  Brown-white
 */
const char iface[] PROGMEM = {
   "i( lagoon, ebutton,\n\
      [ ro( t, int:=369,  \"Temperature\"),\n\
	rw(tt, int:=350,  \"Target Temperature\"),\n\
	ro( f, int:= 0,   \"Fluorescence\"),\n\
        rw(fg, int:=2,    \"Fluorescence Gain\"),\n\
        rw(ft, int:=2,    \"Fluorescence Time\"),\n\
        rw( l, int:=100,  \"Meniscus LED\"),\n\
        rw(ms, int:=100,  \"Mixer Motor Speed\"),\n\
        rw(uv, int:=100,  \"Ultra-violet LED\"),\n\
        rw(ms, int:=100,  \"Mixer Motor Speed\"),\n\
        rw(v1, int:=1000, \"Lagoon Valve 1 Timing\"),\n\
        rw(v2, int:=1000, \"Lagoon Valve 2 Timing\"),\n\
        rw(v3, int:=1000, \"Lagoon Valve 3 Timing\"),\n\
        rw(v4, int:=1000, \"Lagoon Valve 4 Timing\")\n\
      ])." };
      
/* 1) Create valve controller
 * 2) Accept commands from main computer to:
 *     a) adjust timings
 *     b) control meniscus light
 *     c) set auto/manual temperature control
 *     d) set auto/manual mixer control
 *     d) set auto/manual flow control
 * 3) Check temperature and manage lagoon heater
 */
#include "param.h"

void printTermInt(char *f,int a)
{ Serial.print(f);Serial.print("(");Serial.print(a);Serial.println(")."); }
void printTermUInt(char *f, uint16_t a)
{ Serial.print(f);Serial.print("(");Serial.print(a);Serial.println(")."); }
void printTermFloat(char *f,double a)
{ Serial.print(f);Serial.print("(");Serial.print(a);Serial.println(")."); }
	

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
boolean once;
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

#include <Servo.h>
Servo myservo;

boolean debug;
/*
 * Servo write has to be here because this is the main routine and the
 * servo object library has to be included here.  This is Arduino compiler crap.
 * Clearly, the servo control should be inside valve.h (try it, I dare you).
 */

void swrite(int val) {
     if (debug) {
     	Serial.print('servo('),Serial.print(val);Serial.println(').');
     }
     myservo.write(val);
}


#include "valve.h"        // Includes param.h (change constants there)
int valveCycleTime = DEFAULT_CYCLETIME;

VALVE    valve = VALVE(NUM_VALVES+1, VALVEPIN);      // 5-position valve on pin 9

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

int get_temperature()  // Temperature in tenths of degrees C
{
int tries = 4;
int resets = 2;
int tmp;

      while(tries-- > 0)
      {
	    tmp = objTC();
	    if (tmp>100  && tmp<800) return tmp;
	    delayMicroseconds(5000);
	    if ( tries == 0 && resets-- > 0 )
	    {
	       Wire.begin(); delayMicroseconds(5000); tries=4;
	    }
      }
      return 888;
}

boolean auto_temp;   // Automatically control Heater
boolean auto_valve;  // Automatically control Valves
boolean auto_mixer;  // Automatically cycle mixer

/* EEPROM SAVE AND RESTORE OF ID AND CALIBRATION CONSTANTS */

#ifdef STANDALONE
#include "EEPROM.h"
int eeSize;
int RomAddress  = 0;

int mixerspeed;
byte id = 'l'; // Lagoon
int target_temperature;

int interval;   // Variable to keep track of the time
int reading[10];

// Keep temperature within 0.5 degree C

void checkTemperature()
{
int t = get_temperature();
	if (t < target_temperature)      digitalWrite(HEATER,0);
	if (t > target_temperature + 5)  digitalWrite(HEATER,1);
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
	moveData(op, sizeof(int),        (byte *) &target_temperature);
	moveData(op, sizeof(int),        (byte *) &mixerspeed);
	moveData(op, (NUM_VALVES+1)*sizeof(int), valve.getTimesbp());
	moveData(op, (NUM_VALVES+1)*sizeof(byte),valve.getAngles());
	moveData(op, sizeof(int),   (byte *)&valveCycleTime);
	if (op == SAVE) return("save.");
	else            return("restore.");
}
#endif

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
int  *ip = valve.getTimes();

	Serial.println("cmd(a,[0,1],'set auto modes off/on').");
	Serial.println("cmd(cl,'clear backlog (no output)').");
	Serial.print("cmd(cy,[");Serial.print(valveCycleTime);
	Serial.println("],'Valve cycle time').");
	Serial.print("cmd(d,[0,1,2,3,4],[");
	 for(i=0;i<5;i++) { Serial.print(*bp++); if (i != 4) Serial.print(","); }
        Serial.println("],'servo angle:(0-180) for Nth valve position').");
	Serial.println("cmd(e,[0,1],'enable inputs vs. flow calibration').");
	Serial.println("cmd(h,'Print this help message').");
	Serial.println("cmd(h,[0,1],'force heater off/on auto_temp off').");
	Serial.println("cmd(l,[0,1],'light off/on').");
	Serial.println("cmd(m,[0,1],'mixer off/on').");
	Serial.println("cmd(n,'Normal Run mode (valve enabled, valve pos 0, auto_modes on)').");
	Serial.println("cmd(p,[1],[0,1,2,3,4],'set valve to position N, auto_valve off').");
	Serial.println("cmd(r,'Restore settings from EEPROM').");
	Serial.println("cmd(s,'Save settings in EEPROM').");

	Serial.print("cmd(t,[");
	  Serial.print(get_temperature());
	  Serial.println("],'Get temperature in tenth degrees C').");

	Serial.print("cmd(tt,[");
	  Serial.print(target_temperature);
	  Serial.println("],'Get/Set target temperature in tenth degrees C').");

	Serial.print("cmd(v,[1,2,3,4],[");
	 for(i=1;i<5;i++) {
		Serial.print(*ip++); if (i != 4) Serial.print(",");
	}
        Serial.println("],'open time (ms) for Nth valve position').");
	Serial.println("cmd(z,'Zero EEPROM').");
}

void mixer(byte v)
{
	if (v == 0) {
		analogWrite(MIXER,0);
		auto_mixer = false;
	} else {
	    for(int i=3; i<15; i++) {
		analogWrite(MIXER, i*mixerspeed/13);
		if (auto_valve) valve.checkValve();
		delay(400);
 	    }
	    analogWrite(MIXER, mixerspeed-(mixerspeed/10));
	    auto_mixer = true;
	}
}

#define valveRange(c)  ((c) > '0' && (c) < '5')

boolean lagoon_command(char c1, char c2, int value)
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
		     switch(d) {
		     	case 0:
			     auto_temp = false;
			     auto_valve = false;
			     auto_mixer = false;
			     break;
			case 1:
			     auto_temp = true;
			     auto_valve = true;
			     auto_mixer = true;
			     break;
			default:
			     printTermInt("a", auto_valve);
			}
			break;
	   	case 'b':
		        printTermInt("a6", analogRead(A6));
		        break;
	   	case 'c':
		     if ( c2 == 'l' ) return true; // Clearing backlog
		     else if ( c2 == 'y' ) {
			if (value != 0) {
		     	  valveCycleTime = value;
		     	  valve.setCycleTime(value);
			} else
			     printTermInt("cycleTime", valveCycleTime);
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
		        break;
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
		     switch(d) {
		        case 0:
			     digitalWrite(HEATER, 1);
			     break;
			case 1:
			     digitalWrite(HEATER, 0);
			     break;
			default:
			     printHelp();
			}
		case 'i':
			if (c2 == 'd') {
			   if (value == 0)
			      printTermInt("id",id);
			   else
				id = (char) value;
			} else
			  Serial.println(iface);
			break;
		case 'l':
		     switch(d) {
		        case 0:
			     digitalWrite(LED, 1);
			     break;
			case 1:
			     digitalWrite(LED, 0);
			     break;
			default:
			     printTermInt("led",!digitalRead(LED));
			}
			break;
		case 'm':
		        if (c2 == 's') {
				if (value == 0)
				   printTermInt("ms", mixerspeed);
				else {
				   mixerspeed = value;
				   analogWrite(MIXER, mixerspeed );
				}
			}
			else if (d == 9)
			  printTermInt("m",auto_mixer);
			else
			  mixer(d);
			break;
		case 'n':
		        valve.enable(1);
			auto_temp = true;  // Maintain Temperature Control
			auto_valve = true;  // Maintain Flow
			auto_mixer = true;  // Start mixer
			break;
		case 'p':
			if (valveRange(c2)) {
			       // auto_valve = false; dangerous?
				valve.position(c2-'0');
			} else
			        printTermInt("e",(int)c2);
			break;
		case 'r':  
			switch(c2) {
				case 'v': valve.report(reply);
				     	  break;
				default:  strcpy(reply,saveRestore(RESTORE));
					  break;
			}
			break;
		case 's':
			strcpy(reply,saveRestore(SAVE));
			break;
		case 't': 
		        if (c2== 't') {
			   if (value == 0)
			     printTermInt("tt",target_temperature);
			   else
			     target_temperature = value;
			}
		        else {
			    printTermInt("t",get_temperature());
			}
			break;
		case 'v':
		     if (valveRange(c2)) {
		     	int vnum = c2-'0';
		        if (value == 0) {
			   sprintf(reply,"v%c(%d).",c2,valve.getTime(vnum));
			}
			else
			   valve.setup_valve(vnum, value);
	             } else
		           printTermInt("e",(int)c2);
		     break;
		case 'z':
		     EEPROM.write(0,0);
		     strcpy(reply, "z(0).");
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
		if (is.length() > 2) {
			value = atoi(&is[2]);
			if (value == 0) value = 1;
		}
		if (!lagoon_command(is[0], is[1], value))
			Serial.println("e('" + is + "').\nend_of_data");
	}
}

/*
 * setup()	1) Initializes serial link
 *		2) Restores settings from EEPROM
 *		2) Calls flow_setup (pumps)
 *		3) Calls turbid_setup (LED/Optics)
 */

void setup()
{
	Serial.begin(9600);
	debug = false;

	//  Active Low (power to valve) default 1 == no power
	pinMode(HEATER,    OUTPUT);  digitalWrite(HEATER, 1);
	pinMode(LED,       OUTPUT);  digitalWrite(LED, 0);
	pinMode(DRIP,      INPUT);

	pinMode(VALVEDISABLE,OUTPUT);

	//  Enable servo power , move to position 0 and then disable
	digitalWrite(VALVEDISABLE,0);
	delay(300);
	myservo.attach(VALVEPIN);
	valve.position(0);
	delay(500);
	digitalWrite(VALVEDISABLE,1);

        analogWrite(MIXER, 0 );     // Mixer off
	interval = millis();

	eeSize = sizeof(float) + (NUM_VALVES+2)*sizeof(int) + (NUM_VALVES+2)*sizeof(byte);

	if (EEPROM.read(0)==0 || EEPROM.read(0)==255)	// First time
	{
		id = 'l';	// Default Lagoon ID 
		target_temperature = 370;
		mixerspeed = MIXERSPEED;
		valve.setAngle(0,0);
		valve.setup_valve(0, 0);
		valve.setAngle(1,45);
		valve.setup_valve(1, 6000);
		valve.setAngle(2,90);
		valve.setup_valve(2, 3000);
		valve.setAngle(3,139);
		valve.setup_valve(3, 0);
		valve.setAngle(4,180);
		valve.setup_valve(4, 0);
		valveCycleTime = DEFAULT_CYCLETIME;
		saveRestore(SAVE);
	}
	else
	{
		saveRestore(RESTORE); // Valve timing copied to valve object
	}
	luxOn = false;
//        lux_init(2591);
	valve.setCycleTime(valveCycleTime);
	once = true;
	auto_temp = true;   // Maintain Temperature Control
	auto_valve = true;  // Maintain Flow
	auto_mixer = true;  // Cycle magnetic mixer to avoid stalled stir-bar
	if (debug) Serial.println("reset.");

        attachInterrupt(pin2ISR(dripPin), dripCatcher, RISING);
}

int cnt_mixer = 0;
void loop()
{
int t;
        Serial.println(DRIP_count);
	respondToRequest();     // Check for command
	if (auto_valve) valve.checkValve();
	delay(200);
	if (auto_temp)	checkTemperature();

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
}



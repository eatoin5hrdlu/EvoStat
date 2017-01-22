/*
 * Supply controller
 */

const char iface[] PROGMEM = {
   "i( supply, ebutton,\n\
       [ ro( v, int:=100, \"Current Volume\"),\n\
         rw(vt, int:=100, \"Total (Full) Volume\")\n\
       ])."};
      
/* 1) Create supply volume controller
 * 2) Accept commands from main computer to:
 *     a) set/get full volume
 *     b) get current volume
 */
#include "param.h"
boolean debug;

/* EEPROM SAVE AND RESTORE OF ID AND CALIBRATION CONSTANTS */

#ifdef STANDALONE
#include "EEPROM.h"
int eeSize;
int RomAddress  = 0;

byte id = 'n'; // n is default for nutrient supply
int volume = 50;
int full_volume = 100;
int interval;   // Variable to keep track of the time

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
	moveData(op, sizeof(int),        (byte *) &full_volume);
	moveData(op, sizeof(int),        (byte *) &volume);
	if (op == SAVE) return("save.");
	else            return("restore.");
}
#endif

/* Commands:
 *  v  :    Return current volume
 *  vf :    Set current volume as 'FULL'
 *  h  :    Print help
 */

void printHelp(void)
{
	Serial.println("cmd(v,'return the current value').");
	Serial.println("cmd(vf,'set current value as FULL').");
	Serial.println("cmd(h,'Print this help message').");
	Serial.println("cmd(z,'Zero EEPROM').");
}

void printTermInt(char *f,int a)
{ Serial.print(f);Serial.print("(");Serial.print(a);Serial.println(")."); }
void printTermFloat(char *f,double a)
{ Serial.print(f);Serial.print("(");Serial.print(a);Serial.println(")."); }
	
boolean supply_command(char c1, char c2, int value)
{
char reply[80];
byte d;
int tmp;
	switch(c1)
	{
		case 'h':
		     printHelp();
		     break;
		case 'v':
		     if (c2 == 'f') {
		       printTermInt("vf",full_volume);
		     } else
		       printTermInt("v",volume);
		     break;
		case 'z':
		     EEPROM.write(0,0);
		     printTermInt("z",0);
		     break;
		default:
		     printTermInt("e",c1);
		     break;
	}
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
		if (!supply_command(is[0], is[1], value))
			Serial.println("e('" + is + "').\nend_of_data");
	}
}

/*
 * setup()	1) Initializes serial link
 *		2) Restores settings from EEPROM
 */

boolean once;

void setup()
{
	Serial.begin(9600);
	debug = false;
	interval = millis();
	analogReference(INTERNAL);
	eeSize = 1 + 2*sizeof(int);

	if (EEPROM.read(0)==0 || EEPROM.read(0)==255)	// First time
	{
		id = 'n';	// Default Supply ID 
		full_volume = 100;
		volume = 50;
		saveRestore(SAVE);
	}
	else
	{
		saveRestore(RESTORE); // Valve timing copied to valve object
	}
}

void loop()
{
	respondToRequest();     // Check for command
	volume = analogRead(SCALE);
}




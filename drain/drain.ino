#define EOT "end_of_data."

/*
 * Drain/Collector controller
 *
 * 1) Create N drain valve controls
 * 2) Accept commands from main computer to:
 *     a) run auto drain valve timings
 *     b) force drain open/closed
 * Commands:
 *  i  :    Identify yourself
 *  a0 :    Auto mode off
 *  a1 :    Auto modes on (default)
 *  cN :    Close valve N  (forcing auto_drain off)
 *  oN :    Open valve N (forcing auto_drain off)
 *  tdnnnn :   Set open time for drain d to nnnn
 */

#include "drains.h"        // Includes param.h (change constants there)

DRAINS drains = DRAINS(5);

/* EEPROM SAVE AND RESTORE OF ID AND CALIBRATION CONSTANTS */

#include "EEPROM.h"
int RomAddress  = 0;

byte id = 'p'; // p = Generic pump, by default

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
	RomAddress = 0;
	moveData(op, 1, &id);
	moveData(op, drains.getSize(), drains.getPins());
	moveData(op, drains.getSize()*sizeof(int), drains.getTimes());
}

boolean drain_command(char c1, char c2, int value)
{
byte d;
char reply[80];

	switch(c2)
	{
		case '1': d = 1; break;
		case '0': d = 0; break;
		default : break;
	}
	switch(c1)
	{
		case 'a':
		     drains.enable(d);
		     break;
		case 'c':
		     drains.closeValve(c2-'0');
		     break;
		case 'i':
		     if (c2 != 0)
		     	id = c2;
		     else
		     {
			Serial.print("id(");
			Serial.print((char)id);
			Serial.println(").");
		     }
		     break;
		case 'o':
		     drains.openValve(c2-'0');
		     break;
		case 'r':
		     if (c2 == 't') {
		     	drains.report(reply);
		     	Serial.println(reply);
		     } else 
		       saveRestore(RESTORE);
		     break;
		case 's':
		     saveRestore(SAVE);
		     break;
		case 't':
		     drains.setValve(c2-'0',value);
		     break;
		default:
		     return false;
	}
	Serial.println(EOT);
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
		if (!drain_command(is[0], is[1], value)) {
			Serial.println("error('" + is + "').");
			Serial.println(EOT);
		}
	}
}

/*
 * setup()	1) Initializes serial link
 *		2) Restores settings from EEPROM
 *		2) Calls setup
 */

void setup()
{
	Serial.begin(9600); // 9600, 8-bits, no parity, one stop bit
	pinMode(LAGOON1_DRAIN,OUTPUT);
	digitalWrite(LAGOON1_DRAIN,0);
	pinMode(LAGOON2_DRAIN,OUTPUT);
	digitalWrite(LAGOON1_DRAIN,0);
	pinMode(LAGOON3_DRAIN,OUTPUT);
	digitalWrite(LAGOON1_DRAIN,0);
	pinMode(LAGOON4_DRAIN,OUTPUT);
	digitalWrite(LAGOON1_DRAIN,0);
	pinMode(CELLSTAT_DRAIN,OUTPUT);
	digitalWrite(CELLSTAT_DRAIN,0);
//	if (true)
	if (EEPROM.read(0)==0 || EEPROM.read(0)==255)	// First time
	{
		id = 'd';
		drains.setup_valve(0,CELLSTAT_DRAIN,1000);
		drains.setup_valve(1,LAGOON1_DRAIN,4000);
		drains.setup_valve(2,LAGOON2_DRAIN,4000);
		drains.setup_valve(3,LAGOON3_DRAIN,4000);
		drains.setup_valve(4,LAGOON4_DRAIN,4000);
		saveRestore(SAVE);
	}
	else    saveRestore(RESTORE);

        drains.enable(1);
}

void loop()
{
	respondToRequest();     // Respond to commands
	delay(100);
	drains.checkValves();
}

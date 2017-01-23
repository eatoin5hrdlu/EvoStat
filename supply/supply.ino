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
 *     a) set/get full volume	Serial.println("hello...");
 *     b) get current volume
 */

#include "supply.h"
byte id = 'n'; // n is default for nutrient supply
int interval;   // Variable to keep track of the time

SUPPLY supply = SUPPLY(NUM_SUPPLIES);

#define EOT "end_of_data."


/* EEPROM SAVE AND RESTORE OF ID AND CALIBRATION CONSTANTS */

#include "EEPROM.h"
int RomAddress  = 0;

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
	moveData(op, supply.getSize()*sizeof(int), supply.getEmptyArray());
	moveData(op, supply.getSize()*sizeof(int), supply.getFullArray());
	moveData(op, supply.getSize()*sizeof(int), supply.getScaleArray());
}

/* Commands:
 *  v  :    Return current volume
 *  vf :    Set current volume as 'FULL'
 *  h  :    Print help
 */

void printHelp(void)
{
	Serial.println("cmd(v,'{0-4} return volume of vessel N').");
	Serial.println("cmd(f,'{0-4}[<value>] get/set FULL value').");
	Serial.println("cmd(e,'{0-4}[<value>] get/set EMPTY value').");
	Serial.println("cmd(h,'Print this help message').");
	Serial.println("cmd(s,'Save current values in EEPROM').");
	Serial.println("cmd(r,'Restore values from EEPROM').");
	Serial.println("cmd(z,'Zero EEPROM').");
}

void printTermInt(char *f,int a)
{ Serial.print(f);Serial.print("(");Serial.print(a);Serial.println(")."); }
void printTermFloat(char *f,double a)
{ Serial.print(f);Serial.print("(");Serial.print(a);Serial.println(")."); }

boolean in_range(int num) 
{
  if (num > -1 && num <NUM_SUPPLIES)
	return true;
  printTermInt("e",num);
  return false;
}

	
boolean supply_command(char c1, char c2, int value)
{
char reply[80];
byte d;
int num;
char cterm[3];
     cterm[0] = c1;
     cterm[1] = c2;
     cterm[2] = 0;

	switch(c1)
	{
		case 'h':
		     printHelp();
		     break;
		case 'f':
		     num = (int)(c2 - '0');
		     if (in_range(num))
			{
			     if (value == 0)
			       printTermInt(cterm,supply.getFull(num));
			     else
			       supply.setFull(num,value);
			}
		     break;
		case 'e':
		     num = (int)(c2 - '0');
		     if (in_range(num))
			{
			     if (value == 0)
			       printTermInt(cterm,supply.getEmpty(num));
			     else
			       supply.setEmpty(num,value);
			}
		     break;
		case 'r':
		     saveRestore(RESTORE);
		     break;
		case 's':
		     saveRestore(SAVE);
		     break;
		case 'v':
		     num = (int)(c2 - '0');
		     if (in_range(num))
			 printTermInt("v",supply.readVolume(num));
		     break;
		case 'z':
		     EEPROM.write(0,0);
		     printTermInt("z",0);
		     break;
		default:
		     break;
	}
	Serial.println("end_of_data.");	
	return true;
}

void respondToRequest(void)
{
int c;
	String is = "";
	while (Serial.available() > 0)  // Read a line of input
	{
		c  = Serial.read();
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
			Serial.println("e('" + is + "').\nend_of_data.");
	}
}

void setup()
{
	Serial.begin(9600);
	interval = millis();
	analogReference(INTERNAL);
	if (EEPROM.read(0)==0 || EEPROM.read(0)==255)	// First time
	{
		id = 'n';	// Default Supply ID 
		saveRestore(SAVE);
	}
	else
	{
		saveRestore(RESTORE); // Valve timing copied to valve object
	}
}

void loop()
{
	respondToRequest();  // Respond to commands
	delay(100);
        supply.update();
}



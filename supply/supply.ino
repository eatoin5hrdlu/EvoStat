/*
 * Supply controller 
 * Accept commands from main computer to:
 *   a) set/get full/empty volume values
 *   b) get current volume
 */

#include "supply.h"
#define P(x)  Serial.print(x)
#define PL(x) Serial.println(x)

byte id = 'n'; // n is default for nutrient supply
SUPPLY supply = SUPPLY(NUM_SUPPLIES);
#define EOT "end_of_data."

void printInterface()
{
  PL(F("i( supply, ebutton,\n\
       [ rw(f0, int:=1023, \"Full Measurement\"),\n\
	 rw(f1, int:=1023, \"Full Measurement\"),\n\
	 rw(f2, int:=1023, \"Full Measurement\"),\n\
	 rw(f3, int:=1023, \"Full Measurement\"),\n\
	 rw(f4, int:=1023, \"Full Measurement\"),\n\
	 rw(e0, int:=0, \"Empty Measurement\")\n\
	 rw(e1, int:=0, \"Empty Measurement\")\n\
	 rw(e2, int:=0, \"Empty Measurement\")\n\
	 rw(e3, int:=0, \"Empty Measurement\")\n\
	 rw(e4, int:=0, \"Empty Measurement\")\n\
	 ro(v0, int:=128, \"Current Volume\"),\n\
	 ro(v1, int:=128, \"Current Volume\"),\n\
	 ro(v2, int:=128, \"Current Volume\"),\n\
	 ro(v3, int:=128, \"Current Volume\"),\n\
	 ro(v4, int:=128, \"Current Volume\")\n\
       ])."));
}

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
int size = supply.getSize()*sizeof(int);

	RomAddress = 0;
	moveData(op, 1, &id);
	moveData(op, size, supply.getEmptyArray());
	moveData(op, size, supply.getFullArray());
}

void printHelp(void)
{
    PL("cmd(h,'Print this help message').");
    PL("cmd(v,'{0-4} return volume of vessel N').");
    PL("cmd(e,'{0-4}[<value>] get/set EMPTY value').");
    PL("cmd(f,'{0-4}[<value>] get/set FULL value').");
    PL("cmd(c,'{ef}{0-4} empty/full <= current value').");
    PL("cmd(s,'Save current values in EEPROM').");
    PL("cmd(r,'Restore values from EEPROM').");
    PL("cmd(z,'Zero EEPROM').");
}

void printTermInt(char *f,int a){P(f);P("(");P(a);PL(").");}

#define in_range(n) ((num>-1&&num<NUM_SUPPLIES)?true:false)

boolean supply_command(char c1, char c2, int value)
{
int *vp;
int num;
char cterm[3];
     cterm[0] = c1;
     cterm[1] = c2;
     cterm[2] = 0;

	switch(c1)
	{
		case 'c':
		    if (in_range(value))
		    {
			if (c2 == 'e') supply.setEmpty(value);
			else if (c2 == 'f') supply.setFull(value);
	                else return(false);
		    } else return(false);
		     break;
		case 'h':
		     printHelp();
		     break;
		case 'i':
		     printInterface();
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
		     else return(false);
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
 		     else return(false);
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
			 printTermInt("v",supply.getVolume(num));
		     else
                         return(false);
		     break;
		case 'z':
		     EEPROM.write(0,0);
		     printTermInt("z",0);
		     break;
		default:
		     return(false);
		     break;
	}
	PL(EOT);
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
		if (Serial.available() == 0) // wait for input
			delay(100);
	}

	if ( is.length() > 0 )  {   // process the command
		int value = 0;
		if (is.length() > 2)
			value = atoi(&is[2]);
		if (!supply_command(is[0], is[1], value))
			PL("e('" + is + "').\nend_of_data.");
	}
}

void setup()
{
	Serial.begin(9600);
	analogReference(INTERNAL);
	if (EEPROM.read(0)==0 || EEPROM.read(0)==255)
		saveRestore(SAVE);
	else
		saveRestore(RESTORE);
}

void loop()
{
	respondToRequest();  // Respond to commands
	delay(100);
}



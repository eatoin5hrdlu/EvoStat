// One Arduino Mega simulating four Bluetooth devices

#define EOT "end_of_data"
#define SAVE    0
#define RESTORE 1

void saveRestore(int inout) { return; }

int valveTime[5] = { 3000, 4000, 5000, 6000, 7000 };

int who;

#define CELLSTAT    (who == 0)
#define LAGOON      (who >0 && who < 4)

void printHelp0(void) // CELLSTAT
{
	Serial.println("a([0,1],'automode on=1, off=0').");
	Serial.println("cl('clear - purge pipeline (no output)').");
	Serial.println("h('this help text').");
	Serial.println("id(cellstat).");
}
void printHelp1(void)  // LAGOON
{
	Serial1.println("a(value,'aliquot - extraction in msec').");
	Serial1.println("cl('clear - reset platform/start sampling').");
	Serial1.println("d('dump (print) settings').");
	Serial1.println("id(1).");
}
void printHelp2(void)  // LAGOON
{
	Serial2.println("a(value,'aliquot - extraction in msec').");
	Serial2.println("cl('clear - reset platform/start sampling').");
	Serial2.println("d('dump (print) settings').");
	Serial2.println("id(2).");
}
void printHelp3(void)
{
	Serial3.println("a(value,'aliquot - extraction in msec').");
	Serial3.println("cl('clear - reset platform/start sampling').");
	Serial3.println("d('dump (print) settings').");
	Serial3.println("id(3).");
}
void printHelp(void)
{
	switch(who) {
	 case 0  : printHelp0(); break;
	 case 1  : printHelp1(); break;
	 case 2  : printHelp2(); break;
	 case 3  : printHelp3(); break;
	}
}


void setup()
{
	who = -1;
	Serial.begin(9600);
	Serial1.begin(9600);
	Serial2.begin(9600);
	Serial3.begin(9600);
	pinMode(13,OUTPUT);
}

void flash(int n)
{
	while(n--)
	{
	        digitalWrite(13,1);
		delay(300);
		digitalWrite(13,0);
		delay(300);
	}
}

int who_is_ready(void) {
	if (Serial.available()) return 0;
	if (Serial1.available()) return 1;
	if (Serial2.available()) return 2;
	if (Serial3.available()) return 3;
	return -1;
}

int data_available() {
	switch(who) {
	 case 0  : return Serial.available(); break;
	 case 1  : return Serial1.available(); break;
	 case 2  : return Serial2.available(); break;
	 case 3  : return Serial3.available(); break;
	 case -1 : 
	 default : return 0; break;
	}
}

void print_string(char *str) {
	switch(who) {
	 case 0  : Serial.print(str); break;
	 case 1  : Serial1.print(str); break;
	 case 2  : Serial2.print(str); break;
	 case 3  : Serial3.print(str); break;
	}
}
void print_int(int i) {
	switch(who) {
	 case 0  : Serial.print(i); break;
	 case 1  : Serial1.print(i); break;
	 case 2  : Serial2.print(i); break;
	 case 3  : Serial3.print(i); break;
	}
}
void println_string(char *str) {
	switch(who) {
	 case 0  : Serial.println(str); break;
	 case 1  : Serial1.println(str); break;
	 case 2  : Serial2.println(str); break;
	 case 3  : Serial3.println(str); break;
	}
}
	
int read_whoever() {
	switch(who) {
	 case 0  : return Serial.read(); break;
	 case 1  : return Serial1.read(); break;
	 case 2  : return Serial2.read(); break;
	 case 3  : return Serial3.read(); break;
	 case -1 : 
	 default : return 0; break;
	}
}

void respondToRequest(void)
{
	who = who_is_ready();
	if (who < 0) return;
	else flash(who+1);

	String is = "";
	while (data_available() > 0)  // Read bytes
	{
		int c  = read_whoever();
		if ( c < 32 ) break;
		is += (char)c;
		if (data_available() == 0) // Slow down
			delay(200);
	}
	if ( is.length() > 0 )  {   // process the command
		int value = 0;
		if (is.length() > 2)
			value = atoi(&is[1]);
		process(is[0], is[1], value);
	}
}

void printAtom(char *functor)
{ print_string(functor); println_string("."); }

void printTermInt(char *functor, int arg)
{ print_string(functor); print_string("(");
  print_int(arg);
  println_string(").");
}

void printTerm2Int(char *functor, int arg1, int arg2)
{ 
  print_string(functor); print_string("(");
  print_int(arg1);print_string(",");
  print_int(arg2);println_string(")."); 
}

void printTermChar(char *functor, char arg)
{ print_string(functor); print_string("(");
  print_int(arg); println_string(").");
}

void process(char c, char c2, int value)
{
unsigned long time_left;
int temp;
int i;
	switch(c) {
		case 'a':
			break;
		case 'b':
		        printTermInt("turbidity",280);
			break;
		case 'c':
		     if (c2 == 'l') return;
		     break;
		case 'd':
		        printAtom("dump");
			break;
		case 'e':
			break;
		case 'g':
			break;
		case 'h':
			printHelp();
			break;
		case 'i': 
			if (value == 0) printTerm2Int("valve",0,valveTime[0]);
			else		valveTime[0] = value;
			break;
		case 'j': 
			if (value == 0) printTerm2Int("valve",1,valveTime[1]);
			else 		valveTime[1] = value;
			break;
		case 'k': 
			if (value == 0) printTerm2Int("valve",2,valveTime[2]);
			else 		valveTime[2] = value;
			break;
		case 'l': 
			if (value == 0) printTerm2Int("valve",3,valveTime[3]);
			else 		valveTime[3] = value;
			break;
		case 'm': 
			if (value == 0) printTerm2Int("valve",4,valveTime[4]);
			else 		valveTime[4] = value;
			break;
		case 'n':
			break;
		case 'r' :
			saveRestore(RESTORE);
			break;
		case 'p': printTermInt("pump",1);
			break;
		case 's' :
			saveRestore(SAVE);
			break;
		case 't' :
		       printTermInt("temperature",367);
		default :
			printTermChar("ignored",c);
	}
        printAtom(EOT);
}

void loop()
{
	delay(300);
	respondToRequest();
}

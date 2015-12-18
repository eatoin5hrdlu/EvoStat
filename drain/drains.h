#ifndef DRAINS_v1_h
#define DRAINS_v1_h
#define LIBRARY_VERSION	1.0.0
//
// METHODS:
//
// void closeValve( valve ) - Close valve
// int getTime( valve )     - Return the valves "open time" value
// int setTime(int v, int t)- Set valves "open time" value
// void adjust(valve, value)- Add value to the valve's current "open time"
// boolean checkValves()    - Open valves if time for a new cycle and
//                            close valves after their "open time" 
// void openValve(int v)    - Open the valve for its "open time" duration
//

#include "Arduino.h"
#include "param.h"

class DRAINS
{
 public:

  DRAINS(int n) {
    int i;
    size = n;
    current = size;
    for(i=0;i<MAX_VALVES;i++)   // Mark as undefined
      valve_pin[i] = -1;
    // UL below is necessary, this is a bitch! Try #define
    // DEFAULT_CYCLETIME 31 (vs 33), one works the other doesn't!!!

    cycletime = 1000UL * DEFAULT_CYCLETIME; // Cycle in seconds, store as ms
    lasttime = millis();
  }
  
  void enable(int e) {    enabled = e; }

  void setup_valve(int v, byte pn, int tm) {
    cycletime = DEFAULT_CYCLETIME*1000; // Cycle in seconds, store as ms
    valve_time[v] = tm;
    valve_pin[v] =  pn;
    valve_open[v] = 0;
    digitalWrite(pn,0); // Valve off (closed?) by default
  }

  int getSize(void)           { return size;  }
  int getCycletime(void)      { return cycletime/1000;  }
  void setCycletime(int secs) { cycletime = secs*1000; }
  void setValve(int number, int time) { valve_time[number] = time; }

  // report() takes a pointer to a buffer 
   void report(char *reply) {
     char *cp = reply;
     sprintf(cp,"valvetimes([");
     cp += 12;
     for(int i=0; i<size; i++)
       if (valve_pin[i] != -1) {
	 sprintf(cp, "%4d,",valve_time[i]);
	 cp += 5;
       }
     sprintf(cp-1,"]).");
   }

   boolean checkValves(void) {
      unsigned long now = millis();

      if (now > lasttime + cycletime) { // Time to start a drain cycle
	current = 0;
	openValve(current);
	lasttime = millis();
      } else  // Check whether it is time for the next valve
	if (current < size)
	  {
	    if (now > valve_open[current] + valve_time[current])
	      {
		closeValve(current);
		current++;
		if (current < size) 
		  openValve(current);
	      }
	  }
      return true;
   }

// Valve will never open if it's "open time" is zero

  void openValve(int v)
  {
    if (valve_time[v] != 0 && (enabled == 1) )
      {
	digitalWrite(valve_pin[v],1);
	//Serial.print("open(");Serial.print(v);Serial.print(").");

      }
    valve_open[v] = millis(); // when this valve was opened
  }

  void closeValve(int v)
  {
    digitalWrite(valve_pin[v],0);
    // Serial.print("close(");Serial.print(v); Serial.println(").");
    valve_open[v] = 0;
  }

  byte *getTimes()               { return (byte *) &valve_time[0]; }
  byte *getPins()                { return (byte *) &valve_pin[0];  }
  int getTime(int vchar)        { return valve_time[(int)(vchar-'1')]; }
  int setTime(char vchar, int t){ valve_time[(int)(vchar-'1')] = t; }

 private:
  int size;                         // Number of valves
  int current;
  int enabled;
  unsigned long cycletime; // Cycle duration > sum(valve-on-times)
  unsigned long lasttime;  // Beginning of current time interval
  unsigned long valve_open[MAX_VALVES];  // When was the valve opened
  byte          valve_pin[MAX_VALVES];
  int           valve_time[MAX_VALVES];
};
#endif


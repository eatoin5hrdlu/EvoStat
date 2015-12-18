#ifndef VALVES_v1_h
#define VALVES_v1_h
#define LIBRARY_VERSION	1.0.0
// FROM LAGOON2
// The number of valves and initial timings are specified in
// the constructor, rather than having a parameterized object.
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
#define INFLOW  1
#define OUTFLOW 2
#define ALLFLOW (INFLOW|OUTFLOW)
#define NOFLOW  0
#define ENABLED(v) (valve_dir[v]&flow)

class VALVES
{
 public:

  VALVES(int n) {
    int i;
    size = n;
    flow = ALLFLOW;                     // IN and OUT-Flow enabled
    for(i=0;i<n;i++) valve_pin[i] = -1; // Mark as undefined
    cycletime = DEFAULT_CYCLETIME*1000; // Cycle in seconds, store as ms
  }

  void setup_valve(int v, byte pn, int tm, byte dir) {
    valve_time[v] = tm;
    valve_pin[v] =  pn;
    valve_dir[v] = dir;
    valve_open[v] = 0;
    digitalWrite(pn,0); // Valve off (closed?) by default
  }

  void enable_outflow(void)  { flow |= OUTFLOW;  }
  void disable_outflow(void) { flow &= ~OUTFLOW; }
  void enable_inflow(void)   { flow |= INFLOW;   }
  void disable_inflow(void)  { flow &= ~INFLOW;  }

  int getSize(void)           { return size;  }
  int getCycletime(void)      { return cycletime/1000;  }
  void setCycletime(int secs) { cycletime = secs*1000; }

  boolean setValve(int pin, int time) {
    int i;
    for(i=0;i<size;i++) {
      if (valve_pin[i] == pin) {
	valve_time[i] = time;
	return true;
      }
    }
    return false;
  }

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

      if (now > lasttime + cycletime) { // Time to open valves
	for (int i=0; i<size; i++) {
	  openValve(i);                
	  delayMicroseconds(100);  // Spread out switching current surges
	}
	lasttime = millis();

      } else // Check whether it is time to close any valves

	for (int i=0; i<size; i++) {
	  if (valve_open[i] != 0 && (valve_open[i]+valve_time[i]) < now)
	    {
	      closeValve(i);
	    }
	}
      return true;
  }

// Valve will never open if it's "open time" is zero

  void openValve(int v)      {
    if (valve_time[v] != 0 && (valve_dir[v]&flow)) {
      digitalWrite(valve_pin[v],1);
      valve_open[v] = millis(); // when this valve was opened
    }
  }

  void closeValve(int v)     {
    digitalWrite(valve_pin[v],0);
    valve_open[v] = 0;
  }

  int *getTimes()               { return &valve_time[0];   }
  int getTime(int vchar)        { return valve_time[(int)(vchar-'1')]; }
  int setTime(char vchar, int t){ valve_time[(int)(vchar-'1')] = t; }

  void adjust(char vchar, int value) {
    int v = (int)(vchar - '1');
    if (value > 0 && valve_time[v] < cycletime + value)
      valve_time[v] += value;
    else if (value < 0 && valve_time[v] >= abs(value))
      valve_time[v] += value;
  }

 private:
  int size;                         // Number of valves
  byte     flow;
  byte     valve_pin[NUM_VALVES];
  byte     valve_dir[NUM_VALVES];
  int      valve_time[NUM_VALVES];
  long int valve_open[NUM_VALVES];  // When was the valve opened

  unsigned long lasttime;  // Beginning of current time interval
  unsigned long cycletime; // Cycle duration (always > valve on time)

  int testtime;
  int pinPosition(int pin) {
    int i;
    for(i=0;i<size;i++)
      if (valve_pin[i] == pin)
	return i;
    return -1;
  }
  int positinPin(int pos) {
    if (pos < size && valve_pin[pos] != -1)
      return(valve_pin[pos]);
    return -1;
  }
  };
#endif



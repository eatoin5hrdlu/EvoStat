#ifndef VALVE_v1_h
#define VALVE_v1_h
#define LIBRARY_VERSION	1.0.0
// FROM LAGOON2
// The number of valves and initial timings are specified in
// the constructor, rather than having a parameterized object.
//
// METHODS:
//
// void position( valve ) - Go to this position
// int getTime( valve )     - Return the valves "open time" value
// int setTime(int v, int t)- Set valves "open time" value
// void adjust(valve, value)- Add value to the valve's current "open time"
// boolean checkValve()    - Open valves if time for a new cycle and
//                            close valves after their "open time" 
// void openValve(int v)    - Open the valve for its "open time" duration
//
#include "Arduino.h"
//#include "param.h"

class VALVE
{
 public:
  VALVE(int n, int pin) {
    int i;
    size = n;
    disabled = false;
    //  Angles are stored in EEPROM and can now be changed and remembered
    //    for(i=0;i<size;i++)   {
    //      int angle = i*(180/(size-1));  // Angles are 0, 45, 90, 135, 180
    //      valve_angle[i] =  angle;
    //    }
    cycletime = DEFAULT_CYCLETIME*1000; // Cycle in seconds, store as ms
    lastcycle = millis();
  }

  void enable(int d)
  {
    if (d) disabled = false;
    else   disabled = true;
    swrite(valve_angle[0]);
    current = 0;
  }

  void position(int v) {
    swrite(valve_angle[v]);
  }

  void set_drain_pin(int p) {
    drain_pin = p;
  }

  void setup_valve(int v, int tm) {
    valve_open[v] = 0;
    if (v == size-1)
      valve_time[v] = tm;
    else
      valve_time[v] = tm/2;
  }

  int getSize(void)           { return size;  }
  int getCycletime(void)      { return cycletime/1000;  }
  void setCycletime(int secs) { cycletime = secs*1000; }

  // report() takes a pointer to a buffer 
   void report(char *reply) {
     char *cp = reply;
     sprintf(cp,"valvetimes([");
     cp += 12;
     for(int i=0; i<size; i++) {
	 sprintf(cp, "%5d,",valve_time[i]);
	 cp += 6;
     }
     sprintf(cp-1,"]).");
   }

   void next_valve(void)
   {
    if (current == 0)            up = true;  // First half of sequence
    if (up && current == size-1) up = false; // Second half
    valve_open[current] = 0;
    if (up) current = current + 1;
    else    current = current - 1;
    swrite(valve_angle[current]);
    if (current != 0)
      valve_open[current] = millis();
   }

boolean checkValve(void) {
  int i;
  unsigned long now = millis();

  if (disabled)
    return false;
  
   if ((outflowon+outflowms) < now )
     {
       digitalWrite(outflowPin,0);  // OUTFLOW OFF AT END OF outflowms
       outflowon = 0;
     }

  if (current != 0 &&  (now > valve_open[current] + valve_time[current]) )
    next_valve();
  
  if (now > lastcycle + cycletime) // Time to start valve sequence
    {
      lastcycle = millis();
      current = 0;
      next_valve();
      if (calibration != 1) {
	digitalWrite(outflowPin,1);        // OUTFLOW ON AT START OF CYCLE
	outflowon = millis();
      }
    }
  return true;
}

  byte *getTimes()              { return (byte *)&valve_time[0];       }
  int getTime(int vchar)        { return valve_time[(int)(vchar-'0')]; }
  int setTime(char vchar, int t){ valve_time[(int)(vchar-'0')] = t; }

  byte *getAngles()              { return &valve_angle[0];   }
  int setAngle(char vchar, int a){ valve_angle[(int)(vchar-'0')] = a; }
  int setOutflowms(int outms)   { outflowms = outms; }
  int setOutflowpin(int p)      { outflowPin = p; }

  byte *getOutflowms()          { return (byte *)&outflowms; }
  void calibrate(int c)         { calibration = c; }

 private:
  int size;                         // Number of positions
  int current;                      // Current position
  int calibration;                  // Calibration Mode 1=inflow, 2=outflow
  boolean up;
  boolean disabled;
  byte     flow;
  int      valve_time[NUM_VALVES+1];
  byte     valve_angle[NUM_VALVES+1];
  long int valve_open[NUM_VALVES+1];  // When was the valve opened

  unsigned long lastcycle;  // Beginning of current time interval
  unsigned long cycletime; // Cycle duration (always > valve on time)
  unsigned long outflowon;
  int outflowms;
  int outflowPin;
 } ;
#endif



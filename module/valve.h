#ifndef VALVE_v1_h
#define VALVE_v1_h
#define LIBRARY_VERSION	1.0.0
// FROM LAGOON2
// The number of valves and initial timings are specified in
// the constructor, rather than having a parameterized object.
//
// METHODS:
//
// void position( valve )   - Go to this position
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
    cycleCount = 0L;
    cycletime = DEFAULT_CYCLETIME*1000; // Cycle in seconds, store as ms
    lastcycle = millis();
  }

  void enable(boolean d)
  {
    if (!disabled && d) {
      digitalWrite(VALVEDISABLE,1);
      delay(500);
      swrite(valve_angle[0]);
    }
    disabled = !d;
    current = 0;
  }

  void position(int v) {
   digitalWrite(VALVEDISABLE,0);
   delay(1000);
   swrite(valve_angle[v]);
   delay(1000);
   digitalWrite(VALVEDISABLE,1);
  }

  void setup_valve(int v, int tm) {
    valve_open[v] = 0;
    if (v == size-1) valve_time[v] = tm;  /* last position - full time */
    else             valve_time[v] = tm/2;/* intermediates - half time */
  }

  // report() takes a pointer to a buffer 
   void report(char *reply) {
     char *cp = reply;
     sprintf(cp,"valvetimes([");
     cp += 12;
     for(int i=1; i<size; i++) {
	 sprintf(cp, "%5d,",valve_time[i]);
	 cp += 6;
     }
     sprintf(cp-1,"])."); /* Overwrites the last comma */
   }

   void next_valve(void)
   {
    valve_open[current] = 0;
    do
    {
      if      (current == 0) up = true;  // First half of sequence
      else if (up && current == size-1) up = false; // Second half
      if (up) current = current + 1;
      else    current = current - 1;
    } while(    current != 0  // LOOP IF NOT STAYING AT THIS POSITION
	     && valve_time[current] < 2 ); // 0 or 1 = zero time
    if (cycleCount % valve_range[current] == 0) { // Activating Valve
      swrite(valve_angle[current]);
      if (current != 0)
	valve_open[current] = millis();  // Record opening time
    } else { // Reverse when reaching the first valve not activated this cycle
      if (up) up = false;
    }
   } /* End of next_valve(void) */

boolean checkValve(void) {
  int i;
  unsigned long now = millis();
  if (disabled)
    return false;

  if ( current != 0 &&   // If we are in a cycle, and it is time to
       (now > valve_open[current] + valve_time[current]) ) // move on
    {
      next_valve();
    }

  if (now > lastcycle + cycletime - 300) // Time to start valve sequence
    {
      digitalWrite(VALVEDISABLE,0);  // Power up the servo
      cycleCount++;
      current = 0;
      delay(300);
      lastcycle = millis();
      next_valve();
    }

  // If we are back to position zero, but servo is energized, turn it off.
  if (current == 0 && digitalRead(VALVEDISABLE) == 0) {
    delay(400);  // Give it time to reach zero position.
    digitalWrite(VALVEDISABLE,1);
  }
  return true;
}
  /* getTimes() returns pointer to the first valve position time */
  byte *getTimesbp()        { return (byte *)&valve_time[1]; }
  int  *getTimes()          { return         &valve_time[1]; }

  int getTime(int v)        {
    if (v == size-1) return valve_time[v];
    else             return 2*valve_time[v];
  }
  int setTime(int v, int t)  { valve_time[v] = t; }

  byte *getAngles()          { return &valve_angle[0];   }
  byte *getRanges()          { return &valve_range[0];   }
  int setAngle(int v, int a) { valve_angle[v] = a;       }
  int setRange(int v, int a) { valve_range[v] = a;       }
  int getAngle(int v)        { return(valve_angle[v]);   }
  int getRange(int v)        { return(valve_range[v]);   }
  int getSize(void)           { return size;  }
  int getCycleTime(void)      { return cycletime/1000L;   }
  void setCycleTime(int secs) { cycletime = secs * 1000L;}

 private:
  int size;                         // Number of positions
  int current;                      // Current position
  long int cycleCount;
  boolean up;
  boolean disabled;
  byte     flow;
  byte     valve_range[NUM_VALVES+1]; // Valve cycle modulo (only increases with valve num)
  int      valve_time[NUM_VALVES+1];
  byte     valve_angle[NUM_VALVES+1];
  long int valve_open[NUM_VALVES+1];  // When was the valve opened

  unsigned long lastcycle;  // Beginning of current time interval
  unsigned long cycletime; // Cycle duration (always > valve on time)
 } ;
#endif



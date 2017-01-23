#ifndef SUPPLY_v1_h
#define SUPPLY_v1_h
#define LIBRARY_VERSION	1.0.0
//
// METHODS:
//
// void recordEmpty( scale ) - Store the empty reading
// void recordFull( scale ) - Store the full reading
// void setVolume( scale )  - Set the total volume
// void setFullVolume( scale )  - Set the total volume
// void setVolumeUnits( scale, units )  - Set the volume reporting units
// void getVolume( scale )     - Get the current volume
//

#include "Arduino.h"
#include "param.h"

class SUPPLY
{
 public:

  SUPPLY(int n) {
    int i;
    size = n;
    scale_pin[0] = NUTRIENT;
    scale_pin[1] = INDUCER1;
    scale_pin[2] = INDUCER2;
    scale_pin[3] = INDUCER3;
    scale_pin[4] = INDUCER4;
  }
  
  void recordEmpty(int s) { scale_empty[s] = analogRead(scale_pin[s]); }
  void recordFull(int s)  { scale_full[s] = analogRead(scale_pin[s]);  }
  void totalVolume(int s, int volume)  { analogRead(scale_pin[s]);  }

  void recordEmpty(int s, int pn) {
    scale_empty[s] = analogRead(pn);
    scale_pin[s] =  pn;
  }

  int getSize()               { return size;  }

  int getEmpty(int s)         { return scale_empty[s]; }
  int getFull(int s)          { return scale_full[s];  }
  int getReading(int s)       { return analogRead(scale_pin[s]); }

  int setEmpty(int s,int val) { scale_empty[s] = val;  }
  int setFull(int s, int val) { scale_full[s] = val;   }
  int getCycletime(void)      { return cycletime/1000; }

  void setCycletime(int secs) { cycletime = secs*1000; }

  byte *getEmptyArray()     { return (byte *)&scale_empty[0];  }
  byte *getFullArray()      { return (byte *)&scale_full[0];   }
  byte *getScaleArray()     { return (byte *)&scale_volume[0]; }

  void update(void)         {  }

  /*
   *  Total Vol*(current_reading-empty_reading)/(full_reading-empty_reading)
   *
   *   =>    CurrentVolume
   */
  int readVolume(int s)
  {
     if (s >= size) return 0.0;
     double fulldelta = (double) scale_full[s] - (double)scale_empty[s];
     double cdelta = (double) analogRead(scale_pin[s])-(double)scale_empty[s];
     return (int) ( scale_volume[s] * cdelta ) /fulldelta;
  }

 private:
  int size;                         // Number of supplies
  int cycletime;                    // Sampling Frequency
  int    scale_full[NUM_SUPPLIES];    // Input when full
  int    scale_empty[NUM_SUPPLIES];   // Input when empty
  int    scale_volume[NUM_SUPPLIES];  // Volume in mL
  byte   scale_pin[NUM_SUPPLIES];
  byte   scale_units[NUM_SUPPLIES];   // Units for reporting
};
#endif


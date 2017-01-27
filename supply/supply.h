// #include "Arduino.h"
#include "param.h"

class SUPPLY
{
 public:

  SUPPLY(int n) {
    size = n;
    ain[0] = NUTRIENT;
    ain[1] = INDUCER1;
    ain[2] = INDUCER2;
    ain[3] = INDUCER3;
    ain[4] = INDUCER4;
  }
  
  int getSize()        { return size;  }
  int getEmpty(int s)  { return empty[s]; }
  int getFull(int s)   { return full[s];  }
  int getVolume(int s) { return analogRead(ain[s]); }

  int setEmpty(int s)         { empty[s] = ain[s];}
  int setEmpty(int s,int val) { empty[s] = val;   }
  int setFull(int s)          { full[s] = ain[s]; }
  int setFull(int s, int val) { full[s] = val;    }

  /* pointers for Save/Restore to EEPROM */
  byte *getEmptyArray()     { return (byte *)&empty[0];  }
  byte *getFullArray()      { return (byte *)&full[0];   }

 private:
  int size;                        // Number of supplies
  int    full[NUM_SUPPLIES];       // Input when full
  int    empty[NUM_SUPPLIES];      // Input when empty
  byte   ain[NUM_SUPPLIES];        // Analog pin number
};


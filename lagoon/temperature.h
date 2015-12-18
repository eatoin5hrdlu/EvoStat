#ifndef TEMPERATURE_v1_h
#define TEMPERATURE_v1_h
#define LIBRARY_VERSION	1.0.0

#include "Arduino.h"
#include "param.h"
//#include "MLX.h"

#ifndef INTERNAL  // Mega2650 has two references 1.1 and 2.56 (no default 'INTERNAL')
#define INTERNAL INTERNAL2V56
#endif
// Conversions between Analog In(A), Celcius (C), Farenheit (F)

#define FULL_SCALE 1024  // Analog In 0-1023
#define F_OFFSET 32.0
#define F_SCALE_MULT 9.0
#define F_SCALE_DIV 5.0
#define C_TO_F(c) (F_OFFSET+(F_SCALE_MULT*c)/F_SCALE_DIV)
#define F_TO_C(f) (F_SCALE_DIV*((f-F_OFFSET)/F_SCALE_MULT))
#define A_TO_C(a) ((a*110)/1024.0)
#define A_TO_F(a) C_TO_F(A_TO_C(a))

// Constructors:
// TEMPERATURE(A)       LM35-DZ analog temperature IC 10mV/C on pin A
// TEMPERATURE(SCL,SDA) MELEXIS digital PIR, non-contact thermometer
// Methods:
// float c  = celcius()
// float f  = farenheit()
// float getScale()
// float getOffset()
// void setScale(float scale)
// void setOffset(float offset)
//

class TEMPERATURE
{
 public:
#define SAMPLES 10.0

  TEMPERATURE(int pin) {
    aIn = pin;
    analogReference(INTERNAL);
    sCL = -1;
    sDA = -1;  
  }

  TEMPERATURE(int scl, int sda) { aIn = -1; sCL = scl; sDA = sda; }

  float farenheit(void)     { return C_TO_F(celcius()); }
  float celcius(void)
  {
    int sum  = 0;
	for(int i=0; i< SAMPLES; i++) {
		sum += analogRead(ANALOG_TEMPERATURE);
		delay(2);
	}
	return A_TO_C(((float)sum)/SAMPLES);
  }

  float getScale(void)      { return scale;  }
  float getOffset(void)     { return offset; }
  void setScale(float sca)  { scale = sca;   }
  void setOffset(float off) { offset = off;  }

  private:
   int aIn;            // Analog Input pin
   int sCL;            // Melexis clock
   int sDA;            // Melexis data
   float scale;
   float offset;
   float avg;          // average accumulator
  };
#endif


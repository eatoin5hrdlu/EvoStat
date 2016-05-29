#ifndef TEMPERATURE_v1_h
#define TEMPERATURE_v1_h
#define LIBRARY_VERSION	1.0.0

#include "Arduino.h"
#include "param.h"
//#include "MLX.h"

#ifndef INTERNAL  // Mega2650 has two references 1.1 and 2.56 (no default 'INTERNAL')
#define INTERNAL INTERNAL1V1
#endif
// Conversions between Analog In(A), Celcius (C), Farenheit (F)

#define F_OFFSET 320
#define F_SCALE_MULT 9
#define F_SCALE_DIV 5
#define C_TO_F(c) (F_OFFSET+(F_SCALE_MULT*c)/F_SCALE_DIV)
#define F_TO_C(f) ((F_SCALE_DIV*((f-F_OFFSET))/F_SCALE_MULT)
#define A_TO_C(a) (a*multiply/divide)
#define A_TO_F(a) C_TO_F(A_TO_C(a))

// Constructors:
// TEMPERATURE(A)       LM35-DZ analog temperature IC 10mV/C on pin A
// TEMPERATURE(SCL,SDA) MELEXIS digital PIR, non-contact thermometer
// Methods:
// int c  = celcius()   in tenths of degree
// int f  = farenheit()
//

class TEMPERATURE
{
 public:
#define SAMPLES 10

  TEMPERATURE(int pin) {
    aIn = pin;
    analogReference(INTERNAL);  /* 1.1 V == 1023 */
    multiply = 1100;
    divide = 1023;
    sCL = -1;
    sDA = -1;  
  }

  TEMPERATURE(int scl, int sda) { aIn = -1; sCL = scl; sDA = sda; }

  int farenheit(void)     { return C_TO_F(celcius()); }
  int celcius(void)
  {
    sum = 0;
    for(int i=0; i < SAMPLES; i++) {
	  sum += analogRead(ANALOG_TEMPERATURE);
	  delay(2);
	}
	return A_TO_C(sum/SAMPLES);
  }

  private:
   int aIn;            // Analog Input pin
   int sCL;            // Melexis clock
   int sDA;            // Melexis data
   unsigned long int multiply;
   unsigned long int divide;
   unsigned long int sum;          // average accumulator
  };
#endif


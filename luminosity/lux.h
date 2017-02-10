#ifndef LUX_v1_h
#define LUX_v1_h
#define LIBRARY_VERSION	1.0.0

/* TSL2591 Digital Light Sensor, 600M:1 dynamic range, 88K max lux */
#include <Wire.h>
#include <Adafruit_Sensor.h>
#include "Adafruit_TSL2591.h"

void unifiedSensorAPIRead(void)
{
}
/*
 * Configure gain and integration time for the TSL2591
 */


void setup(void) 
{
  Serial.begin(9600);
  Serial.println("Starting Adafruit TSL2591 Test!");
}

void simpleRead(void)
{
// Simple data of infrared, full, or 'visible' (difference between the two)
// channels. Can take 100-600 milliseconds. Uncomment desired spectra.

   Serial.print("[ "); Serial.print(millis()); Serial.print(" ms ] ");
   Serial.print("Luminosity: ");
   Serial.println(x, DEC);
}

#include "Arduino.h"
#include "param.h"

class LUX
{
 public:
#define SAMPLES 10.0

  LUX(void) {
    sCL = 5;
    sDA = 4;  
    tsl  = Adafruit_TSL2591(2591); // pass in id (for you later)
    if (tsl.begin())
      Serial.println("Found a TSL2591 sensor");
    else
      Serial.println("No sensor found ... check your wiring?");
    configureSensor(2,3);
  }

  void configureSensor(int g, int t)
  {
    if (g < 0 || g > 3 || t < 0 || t > 5)
      {
	Serial.println("gain or integration time out of range");
	return;
      }
    tsl.setGain(gain[g]);
    tsl.setTiming(timing[t]);
  }

  int getGain(void) // 0-3
  {
    int i;
    tsl2591Gain_t gainval = tsl.getGain();
    for(i=0;i<4;i++)
      if (gainval == gain[i]) return i;
    return -1;
  }

  int getTiming(void) // mSeconds
  {
    return((tsl.getTiming() + 1)*100);
  }

  void details()
  {
    sensor_t sensor;
    tsl.getSensor(&sensor);
    Serial.print  ("Sensor:    "); Serial.println(sensor.name);
    Serial.print  ("Driver Ver:"); Serial.println(sensor.version);
    Serial.print  ("Unique ID: "); Serial.println(sensor.sensor_id);
    Serial.print  ("Max Value: "); Serial.print(sensor.max_value);
    Serial.println(" lux");
    Serial.print  ("Min Value: "); Serial.print(sensor.min_value);
    Serial.println(" lux");
    Serial.print  ("Resolution:"); Serial.print(sensor.resolution);
    Serial.println(" lux");  
    Serial.println("------------------------------------");
  }

  uint16_t getLux(void)
  {
    return(tsl.getLuminosity(TSL2591_VISIBLE));
    return(tsl.getLuminosity(TSL2591_FULLSPECTRUM));
    return(tsl.getLuminosity(TSL2591_INFRARED));
  }
  float getGain(void)      { return gain;  }
  float getfloor(void)     { return offset; }

  private:
   Adafruit_TSL2591 tsl;
   int sCL;     // clock
   int sDA;     // data
   int gain[4] = { TSL2591_GAIN_LOW,   //    1X
		   TSL2591_GAIN_MED,   //   25X
		   TSL2591_GAIN_HIGH,  //  428X
		   TSL2591_GAIN_MAX }; // 9876X

   int timing[6] = { TSL2591_INTEGRATIONTIME_100MS,
		     TSL2591_INTEGRATIONTIME_200MS,
		     TSL2591_INTEGRATIONTIME_300MS,
		     TSL2591_INTEGRATIONTIME_400MS,
		     TSL2591_INTEGRATIONTIME_500MS,
		     TSL2591_INTEGRATIONTIME_600MS };
   int floor;
   float avg;   // average accumulator
  };
#endif


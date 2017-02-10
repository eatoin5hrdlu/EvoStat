#ifndef LUX_v1_h
#define LUX_v1_h
#define LIBRARY_VERSION	1.0.0

/* TSL2591 Digital Light Sensor, 600M:1 dynamic range, 88K max lux */
#include <Wire.h>
#include <Adafruit_Sensor.h>
#include "Adafruit_TSL2591.h"

class LUX
{
 public:
#define SAMPLES 10.0

  LUX(int id) {
    tsl  = Adafruit_TSL2591(id); // pass in id (for you later)
    if (tsl.begin())
      Serial.println(F("Found TSL2591"));
    else
      Serial.println(F("No sensor"));
    configureSensor(2,3);
  }

  void configureSensor(int g, int t)
  {
    if (g < 0 || g > 3 || t < 0 || t > 5)
      {
	Serial.println(F("gain/integration time out of range"));
	return;
      }
    tsl.setGain(gain[g]);
    tsl.setTiming(timing[t]);
  }

  int luxGain(void) // 0-3
  {
    int i;
    tsl2591Gain_t gainval = tsl.getGain();
    for(i=0;i<4;i++)
      if (gainval == gain[i]) return i;
    return -1;
  }

  int luxTiming(void) // mSeconds
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
    return(tsl.getLuminosity(TSL2591_FULLSPECTRUM));
    //    return(tsl.getLuminosity(TSL2591_VISIBLE));
    //    return(tsl.getLuminosity(TSL2591_INFRARED));
  }

  private:
   Adafruit_TSL2591 tsl;
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


/* TSL2591 Digital Light Sensor */
/* Dynamic Range: 600M:1 */
/* Maximum Lux: 88K */

#include <Wire.h>
#include <Adafruit_Sensor.h>
#include "Adafruit_TSL2591.h"

Adafruit_TSL2591 tsl = Adafruit_TSL2591(2591); // pass in id (for you later)

void displaySensorDetails(void)
{
  sensor_t sensor;
  tsl.getSensor(&sensor);
  Serial.println("------------------------------------");
  Serial.print  ("Sensor:       "); Serial.println(sensor.name);
  Serial.print  ("Driver Ver:   "); Serial.println(sensor.version);
  Serial.print  ("Unique ID:    "); Serial.println(sensor.sensor_id);
  Serial.print  ("Max Value:    "); Serial.print(sensor.max_value); Serial.println(" lux");
  Serial.print  ("Min Value:    "); Serial.print(sensor.min_value); Serial.println(" lux");
  Serial.print  ("Resolution:   "); Serial.print(sensor.resolution); Serial.println(" lux");  
  Serial.println("------------------------------------");
  Serial.println("");
  delay(500);
}
void unifiedSensorAPIRead(void)
{
}
/**************************************************************************/
/*
    Configures the gain and integration time for the TSL2591
*/
/**************************************************************************/
void configureSensor(void)
{
  // You can change the gain on the fly, to adapt to brighter/dimmer light situations
  //tsl.setGain(TSL2591_GAIN_LOW);    // 1x gain (bright light)
  //tsl.setGain(TSL2591_GAIN_MED);      // 25x gain
  tsl.setGain(TSL2591_GAIN_HIGH);   // 428x gain
  // tsl.setGain(TSL2591_GAIN_MAX);   // 9876x gain
  
  // Changing the integration time gives you a longer time over which to sense light
  // longer timelines are slower, but are good in very low light situtations!
 tsl.setTiming(TSL2591_INTEGRATIONTIME_100MS);  // shortest integration time (bright light)
  // tsl.setTiming(TSL2591_INTEGRATIONTIME_200MS);
  // tsl.setTiming(TSL2591_INTEGRATIONTIME_300MS);
  // tsl.setTiming(TSL2591_INTEGRATIONTIME_400MS);
   // tsl.setTiming(TSL2591_INTEGRATIONTIME_500MS);
  // tsl.setTiming(TSL2591_INTEGRATIONTIME_600MS);  // longest integration time (dim light)

  /* Display the gain and integration time for reference sake */  
  Serial.println("------------------------------------");
  Serial.print  ("Gain:         ");
  tsl2591Gain_t gain = tsl.getGain();
  switch(gain)
  {
    case TSL2591_GAIN_LOW:
      Serial.println("1x (Low)");
      break;
    case TSL2591_GAIN_MED:
      Serial.println("25x (Medium)");
      break;
    case TSL2591_GAIN_HIGH:
      Serial.println("428x (High)");
      break;
    case TSL2591_GAIN_MAX:
      Serial.println("9876x (Max)");
      break;
  }
  Serial.print  ("Timing:       ");
  Serial.print((tsl.getTiming() + 1) * 100, DEC); 
  Serial.println(" ms");
  Serial.println("------------------------------------");
  Serial.println("");
}

void setup(void) 
{
  Serial.begin(9600);
  Serial.println("Starting Adafruit TSL2591 Test!");
  if (tsl.begin()) 
     Serial.println("Found a TSL2591 sensor");
  else
    Serial.println("No sensor found ... check your wiring?");
  displaySensorDetails();
  configureSensor();
}

void simpleRead(void)
{
// Simple data of infrared, full, or 'visible' (difference between the two)
// channels. Can take 100-600 milliseconds. Uncomment desired spectra.

//uint16_t x = tsl.getLuminosity(TSL2591_VISIBLE);
uint16_t x = tsl.getLuminosity(TSL2591_FULLSPECTRUM);
// uint16_t x = tsl.getLuminosity(TSL2591_INFRARED);

   Serial.print("[ "); Serial.print(millis()); Serial.print(" ms ] ");
   Serial.print("Luminosity: ");
   Serial.println(x, DEC);
}

void loop()
{
	delay(1000);
	simpleRead();
}



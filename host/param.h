#define STANDALONE 1

#define SAVE	1          // EEPROM STORAGE COMMANDS
#define RESTORE	0

#define ANALOG_TEMPERATURE   A0 // Analog input for temperature
#define ANALOG_LEAK          A1 // Analog input for leak detection
#define ANALOG_TURBIDITY     A3 // Turbidity (brown/white)
                                // +5V for photo-trans (brown)
// WIRING                       // Pullup for 600nm LED (blue)
                                // Ground LED (blue/white)
                                // Ground  ( green )
#define SPI_CLOCK_PIN        A4 // SCL  ( green/white )
                                // +5V  (orange)
#define SPI_DATA_PIN         A5 // SDA  ( orange/white )

#define AIR            10     // Control solid-state relay (~120VAC)
#define HEATER          9     // Control solid-state relay (~40VAC)
#define LED             13    // Arduino LED
#define LASER           11    // Turbidity LASER (PWM on Nano or Mega)
#define JARLIGHT        7     // Meniscus light
#define NUTRIENT        4     // V0 is Nutrient supply
#define INDUCE1         5     // 
#define INDUCE2         8     // 
#define HOSTOUT         5     // Output to Waste
#define MIXER           3     // PWM for 5V motor

#define DEFAULT_CYCLETIME       20  // Seconds

#define NUM_VALVES      3
#define MAX_VALVES      6

#define MIXERSPEED 80   // PWM value for top mixer speed



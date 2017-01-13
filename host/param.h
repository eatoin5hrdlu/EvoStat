#define STANDALONE 1

#define SAVE	1          // EEPROM STORAGE COMMANDS
#define RESTORE	0

#define ANALOG_TEMPERATURE   0 // Analog input for temperature
#define ANALOG_LEAK          1 // Analog input for leak detection
#define ANALOG_TURBIDITY     3 // Analog input for turbidity

#define AIR            10     // Control solid-state relay (~120VAC)
#define HEATER          9     // Control solid-state relay (~40VAC)
#define LED             13    // Arduino LED
#define LASER           11    // Turbidity LASER (PWM on Nano or Mega)
#define JARLIGHT        7     // Meniscus light
#define NUTRIENT        4     // Only one valve for Cellstat
#define HOSTOUT         5     // Output to Waste
#define MIXER           3     // PWM for 12V motor

#define DEFAULT_CYCLETIME       20  // Seconds

#define NUM_VALVES      3
#define MAX_VALVES      6

#define MIXERSPEED 80   // PWM value for top mixer speed



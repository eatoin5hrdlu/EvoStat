#define STANDALONE 1

#define SAVE	1          // EEPROM STORAGE COMMANDS
#define RESTORE	0

#define ANALOG_TEMPERATURE   0 // Analog input for temperature

#define NUM_VALVES      4

#define VALVEPIN        8    // Control of valve servo
#define VALVEDISABLE    9    // Power to valve servo 

#define LED            10     // Meniscus light
#define HEATER         11     // Power resistors
#define DRIP            2     // Interrupt Drip
#define MIXER           3     // PWM for 5V motor

#define MIXERSPEED   120      // PWM (5v) value for top mixer speed

#define DEFAULT_CYCLETIME 30L // Can be changed/stored in EEPROM


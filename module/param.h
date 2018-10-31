#define STANDALONE 1

#define SAVE	1          // EEPROM STORAGE COMMANDS
#define RESTORE	0

#define ANALOG_LEAK          A1 // Analog input for leak detection
#define ANALOG_TURBIDITY     A3 // Turbidity (brown/white)
#define SPI_CLOCK_PIN        A4 // SCL
#define SPI_DATA_PIN         A5 // SDA

#define NUM_VALVES      4

#define DRIP            2     // Interrupt Drip Beam
#define MIXER           3     // PWM for 5V motor
#define TBOOST          4     // Extra Heat (hair dryer)
#define WASTE           5     // Output to Waste (12V Solenoid Valve)
#define DRIP2           6     // Aux Interrupt Drip
#define JARLIGHT        7     // Meniscus light
#define VALVEPIN        8    // Control of valve servo
#define VALVEDISABLE    9    // Power to valve servo 
#define AIR            10    // Control solid-state relay (~120VAC)
#define HEATER         11    // Resistive Heating: 2 X 12.5 ohms = 4W
#define LED600         12    // 600nM LED-
#define LED            13    // Arduino LED

#define DEFAULT_CYCLETIME       20L  // Seconds

#define MIXERSPEED 80   // PWM value for top mixer speed


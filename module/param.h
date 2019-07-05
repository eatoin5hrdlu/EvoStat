#define STANDALONE 1

#define SAVE	1          // EEPROM STORAGE COMMANDS
#define RESTORE	0

// SENSOR CONNECTIONS SC
// MODULE CONNECTIONS MC
// DIRECT CONNECTIONS OTHER

#define CURRENT_SENSE        A0 // Analog output
#define ANALOG_LEAK          A1 // Analog leak detection OTHER
#define CURRENT_SENSE2       A2 // Analog input
#define ANALOG_TURBIDITY     A3 // Turbidity (brown/white) SC
#define SPI_DATA_PIN         A4 // SDA
#define SPI_CLOCK_PIN        A5 // SCL
#define AUX1                 A6
#define UVC                  A7

#define NUM_VALVES      4

// Only 3 PWM Nano: [3 int],5,6,[9,10 servo],11
#define DRIP            2    // Interrupt Drip Beam     SC
#define DRIP2           3    // Aux Interrupt Drip      SC
#define AIR             4    // AIR PUMP
#define MENISCUS        5    // Meniscus light     PWM  MC
#define MIXER           6    // PWM for 5V motor   PWM  MC
#define WASTE           7    // Output to Waste (12V Solenoid Valve)
#define VALVEPIN        8    // Control of valve servo
#define VALVEDISABLE    9    // Power to valve servo 
#define LASER          10    // Powers all lasers
#define LED600         11    // 600nM LED-  (PWM)      SC
#define HEATER         12    // Resistive Heating: 2 X 12.5 ohms = 4W
#define LED            13    // Arduino LED

#define DEFAULT_CYCLETIME       20L  // Seconds

#define MIXERSPEED 80   // PWM value for top mixer speed


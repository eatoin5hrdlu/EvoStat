#ifndef PID_v1_h
#define PID_v1_h
#define LIBRARY_VERSION	1.0.0

class PID
{
  public:
  //Constants used in some of the functions below
  #define AUTOMATIC	1
  #define MANUAL	0
  #define DIRECT        0
  #define REVERSE       1

    PID(double*, double*, double*,        // Constructor. Link PID to variables
        double, double, double, int);     //  Tuning params
	
    void SetMode(int Mode);               // * sets PID to Manual(0) or Auto(1)
    bool Compute();                       // PID calculation called from loop()
    void SetOutputLimits(double, double); //clamps output range: default 0-255

    // Not commonly used
    void SetTunings(double,               // Usually set tunings in constructor
		    double,               // but
                    double);         	  // this allows adaptive control

    void SetControllerDirection(int);	  // Sets the Direction/Action
    //   DIRECT means the output will increase when error is positive.
    //   REVERSE means the opposite. Unlikely to change during operation.
    void SetSampleTime(int);              // Frequency of calculation in mSec

    //Display functions
    double GetKp();
    double GetKi();
    double GetKd();
    int GetMode();
    int GetDirection();					  //

  private:
	void Initialize();
	
	double dispKp;	          // Tuning parameters in user-entered 
	double dispKi;            //   format for display purposes
	double dispKd;
    
	double kp;                // * (P)roportional Tuning Parameter
	double ki;                // * (I)ntegral Tuning Parameter
	double kd;                // * (D)erivative Tuning Parameter

	int controllerDirection;

	double *myInput;          // Pointers to variables
	double *myOutput;         // Creates link between the vars and PID
	double *mySetpoint;
			  
	unsigned long lastTime;
	double ITerm, lastInput;

	unsigned long SampleTime;
	double outMin, outMax;
	bool inAuto;
};
#endif


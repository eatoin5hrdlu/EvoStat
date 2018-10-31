const int dripPin[2] = { DRIP, DRIP2 };  // PINS with ISR for photo-interrupter

volatile int     DRIP_count[2];
volatile int     DRIP_events[2];
volatile long    DRIP_time[2];
int              last_DRIP_count[2];
int              DRIP_DATA[2][5];
int              DD_cnt;

void reportDRIP(void)
{
  Serial.print("dd([");
  for(int i=0; i<2; i++)
    {
      for(int n=0; i<DD_cnt; i++)
	{
	  Serial.print(DRIP_DATA[i][n]);
	  if (i<1 || n<(DD_cnt-2)) Serial.print(",");
	}
    }
  Serial.println("]).");
}

void dripCatcher0() // 0 -> 1 transition on pin 2
{
long now = millis();
     DRIP_events[0]++;                 // Two events <10mS
     if ( (now - DRIP_time[0]) > 10 )  // is the same drop
     	DRIP_count[0]++;
     DRIP_time[0] = now;
}

void dripCatcher1() // 0 -> 1 transition on pin 6
{
long now = millis();
     DRIP_events[1]++;                 // Two events <10mS
     if ( (now - DRIP_time[1]) > 10 )  // is the same drop
     	DRIP_count[1]++;
     DRIP_time[1] = now;
}


long  DROP_timer = 0;

// Return the flow rate in mL/hour
// 1) An interrupt handler counts the drops
// 2) Once per minute we compute the rate in mL/hour 
// 3) flowRate() returns the last computed value (e.g. value updated once per minute)
//
//  60 minutes/hour * Drops-per-minute / 20 drops per mL  =   (min/hr)((d/min)/(d/mL)) = mL/hr
//  (60/20) =  3*drops-per-min/drops-per-mL = mL/hr

int flowRate(int ch)   // Channel number [0,1] 20/mL Adjust if necessary
{
static int flow_rate[2] = { 40, 10 }; 
long now = millis();

     if ( (now - DROP_timer) > 60000 )  // One minute has passed
     {
       noInterrupts();
       for (int i=0; i<2; i++)
	 {
	   flow_rate[i] = DRIP_count[i]*3; // (divide by 20 for mL/min X 60 for mL/hour)
	   DRIP_DATA[i][DD_cnt] = DRIP_count[i];
	   DRIP_count[i] = 0;
	 }
       interrupts();              // Fast enough so we don't miss a drop
       if (DD_cnt < 5) DD_cnt++;
       DROP_timer = now;
     }
return flow_rate[ch];
}

int iflowRate(int ch)   // Instantaneous
{
int dps;
       noInterrupts();
       dps = DRIP_count[ch];
       interrupts();
return dps;
}


#ifndef WIFI_v1_h
#define WIFI_v1_h
#define LIBRARY_VERSION	1.0.0
#define TILDE 126 

/*
 * Wifi submodule (alternative for Bluetooth)
 *
 * 1) Configure ESP8266 WiFi connection server and wait to be contacted
 *    WIFI w = WIFI();
 *         w.start_server();    
 *
 * 2) Accept connection and respond to requests
 *         w.myrecv(&c1,&c2,&val) fills in 2-chars and an int
 *         w.mysend(char *cp)     send text over link
 */

class WIFI
{
 private:
  int id;           // -1 not connected, 0-10 WiFi, 100 Bluetooth
  int okstate;
  int tover;        // Count the number of forced restarts
  bool once;
  int  ap;          // Index of successfully connected access point
  char buf[100];    // Arduino Serial only has 64-byte buffer
  unsigned long lastcomm; // mS since last interaction for (3m) timeout

 public:
  WIFI() {
    ap = -1;
    id = -1;
    once = true;
    lastcomm = millis();
    tover=0;
    okstate=0;
  }

  bool mysend(const char *data)
    {
      bool sent;
      int tries = 10;
      int cntr = 0;
      if (id > -1) // Connected
	{
	  Serial.print("AT+CIPSEND=");
	  Serial.print(id);
	  Serial.print(",");
	  Serial.println(strlen(data)+2);
	  while (!Serial.available() && cntr++ < tries)
	    delay(200);
	  cntr = 0;
	  while(Serial.available() &&
		cntr++ < tries     &&
		Serial.read() != '>' ) delay(200);
	  Serial.println(data);
	  return okResponse(1000, 2);
	}
      return false; // no connection yet
    }

  // Every line must end with \n ( \r ignored )
  // Waits up to delay mS for some data to appear
  bool readline(int dly)
    {
    int c;
    int i = 0;
    //    delay(dly);
    // int incr = ( dly > 100 ? dly/100 : 10);
    //    while (!Serial.available() && i < dly) { delay(incr); i += incr; }
    //    i = 0;
    while ( Serial.available() && i < sizeof(buf) )
      {
	c  = Serial.read();
	if ( c == 13 ) continue;
	if ( c == 10 ) break;
	buf[i++] = (char)c;
	if (Serial.available() == 0) // It is possible we're too fast
	  delay(200);
      }
    buf[i] = NULL;
    if (i == 0) return false;
    return true;
  }

  /*
   * Only gets called if Serial.available() is true
   */
  bool myrecv(char *pc1, char *pc2, int *pvalue)
  {
    bool restart = false;
    int len, conid;
    *pc1 = TILDE;
    *pc1 = TILDE;
    *pvalue = 0;
    if (readline(1000)) // Fills buf or returns false
      {
	if ( sscanf(buf,"+IPD,%d,%d:%c%c%d",&id,&len,pc1,pc2,pvalue) > 2)
	  {
	    lastcomm = millis();  // Reset timeout
	    return true;
	  }
	if ( sscanf(buf,"%d,CLOSED", &conid) == 1 )
	  {
	    restart = true;
	  }
      }
      if ( (millis() - lastcomm) > 480000 ) // 8 minute timeout!
	restart = true;
      if (restart)
	reboot(1);
      return false;
  }

  void reboot(int f)
  {
    flash(f);
    closeConnection();
    delay(1000);
    id = -1;
    start_wifi();   // Just connection server (was start_wifi)
    lastcomm = millis();
    tover = tover + 1;
  }

  bool connected() { return (id > -1); }
  int  turnover()  { return tover;     }
  
  int accept()
  {
    if (readline(1000)) {
      if ( sscanf(buf, "%d,CONNECT", &id) > 0 )
	flash(3);
      else
	flash(1);
    }
    return id;
  }

  /*
   * State machine to recognize OK\r\n responses
   */
  char sread()
  {
    int c = Serial.read();
    if      (okstate == 0 && c == 79 ) okstate = 1;
    else if (okstate == 1 && c == 75 ) okstate = 2;
    else if (okstate == 2 && c == 13 ) okstate = 3;
    else if (okstate == 3 && c == 10 ) okstate = 4;
    else okstate = 0;
    return (char) c;
  }

  bool okReceived() { return okstate == 4; }

  /*
   * Read input into buf watching for "OK\r\n"
   * okResponse will not read past the first "OK"
   */
  bool okResponse(int dly, int n)
    {
    int waiting = 0;
    okstate = 0;
    while (waiting < dly)  // Give the data a chance to arrive
      {
	if ( dly > 4000 ) { delay(dly); waiting = dly; }
	else              { delay(200);	waiting += 200;}
	if (Serial.available() > 0) waiting = dly;
      }
    int len = 0;
    while (Serial.available() > 0 && len < sizeof(buf))
	{
	  buf[len++] = sread();
	  if (okReceived()) { flash(n);	return true; }
	  if (Serial.available() == 0)
	    delay(200);
	}
    buf[len] = NULL;
    flash(1);
    return false;
  }

  void flash(int n)
  {
    int i;
    int dly = ( n==1 ? 1000 : 100 ); // One long or N short flashes
    for (i=0;i<n;i++) {
      digitalWrite(LED,1);  delay(dly-50);
      digitalWrite(LED,0);  delay(dly+50);
    }
  }

  bool closeConnection()
  {
    bool ok;
    Serial.print("AT+CIPCLOSE=");
    Serial.println(id);
    ok = okResponse(1000, 9);
    if (ok) id = -1;
    return ok;
  }

  bool atcmd(char *cmd, int dly, int numflash) 
  {
    if (strncmp(cmd,"AT",2)) 
      Serial.print("AT+");
    Serial.println(cmd);
    return okResponse(dly, numflash);
  }

  bool joinAP() // Join first available, or known good Access Point
  {
    if (ap > -1) return (atcmd(secrets[ap].joinap, 12000, 5));
    
    int s;
    for (s=0; s < NUM_SECRETS; s++)
      if ( atcmd(secrets[s].joinap, 12000, 5) )
	{
	  ap = s;
	  return true;
	}
    return false;
  }

  void start_wifi()
  {
    bool rst = true;
    while(rst)
      {
	atcmd("RST", 6000, 2);
	atcmd("ATE0", 1000, 4);
	atcmd("CIPMODE=0", 1000, 2);
	atcmd("CWAUTOCONN=0",1000, 4);
	atcmd("CWQAP", 1000, 2);
	atcmd("CWMODE=3",1000, 4);
	if ( joinAP() )
	  {
	    atcmd("CIPMUX=1", 1000, 2);
	    atcmd("CIPSERVER=0", 1000, 4);
	    rst = !atcmd("CIPSERVER=1,23",4001, 3);;
	  }
      }
 }

};  // End of WIFI Class
#endif


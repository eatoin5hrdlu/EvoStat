typedef struct secrets_t {
  char *ssid;
  char *joinap;
} secrets_t;

#define NUM_SECRETS 6
secrets_t secrets[NUM_SECRETS] = {
  { "milton",         "CWJAP=\"milton\",\"\"" },
  { "Museum-Business","CWJAP=\"Museum-Business\",\"DinoDig11\""  },
  { "splatspace",     "CWJAP=\"splatspace\",\"hacktheplanet\""  },
  { "UNC-PSK",        "CWJAP=\"UNC-PSK\",\"IDontWantChangeIWantSwissCheese\""  },
  { "Museum_Guest",   "CWJAP=\"Museum_Guest\",\"\""  }
};




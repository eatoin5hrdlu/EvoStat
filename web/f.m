% LAGOON MODEL:  FOUR COUPLED DIFFERENTIAL EQUATIONS:
%
% UNINFECTED  -> ADSORBED  -> PRODUCTIVE  ->  PHAGE
%  (cells)        (cells)      (cells)      (particles)

function xdot = f(x,t)
  global kg ad ec pp fr h0 vol;
  pm = 1; % This is what makes the g-model different (from f)

  lkg = delta(kg, 0.02*pm); % Local (varied) normal E. coli growth constant
  lad = delta(ad, 0.1*pm);  % Local (varied) adsorption coefficient
  lec = delta(ec, 0.1*pm);  % Local (varied) rate exiting eclipse
  lpp = delta(pp, 0.05*pm);  % Local (varied) phage production per cell
  dc = fr; % new variable so we could model dynamic modification

% Inhibited growth rates for subsequent stages of E. coli
% Ultimately this should be measured for given strains of E. coli and M13
  inhibit = 0.9;
% Infected cells have inhibited growth rate relative to healthy host cells
  kgp = lkg*inhibit;
% Productive cells have inhibited growth rate relative to merely infected cells
  kgpp = kgp*inhibit;

% INFLOW = Flow rate in volumes/hour * concentration cells/ML * volume mL = number of cells
% Growth = Number of Cells * Growth constant (doubling time?)             = number of cells
% Infection = (Total Number of Cells * Total Number of Phage * Adsorption Constant)/total Volume = Fraction Infected
  
% (1) Uninfected Host flowing in at Dilution Constant * Cells/mL
%           + INFLOW       +   GROWTH   - OUTFLOW  - TRANSFORMATION
  xdot(1) =   dc*h0*vol    +  lkg*x(1)  - dc*x(1)  - (lad*x(1)*x(4))/vol;

% (2) Infected Host (previous TRANSFORMED is this equation's INFLOW)
  xdot(2) =  (lad*x(1)*x(4))/vol +  kgp*x(2)  - dc*x(2)  -  lec*x(2);

% (3) Productive Host: INFLOW from Eclipse state, plus growth, minus dilution 

  xdot(3) =  lec*x(2)      + kgpp*x(3)  - dc*x(3);  % No transformation output
  
% (4) Phage Production:  No Growth      - OUTFLOW   % No transformation output

  xdot(4) =  lpp*x(3)                   - dc*x(4);
  
endfunction


% LAGOON MODEL:  FOUR COUPLED DIFFERENTIAL EQUATIONS:
%
% UNINFECTED  -> ADSORBED  -> PRODUCTIVE  ->  PHAGE
%  (cells)        (cells)      (cells)      (particles)

function xdot = g2(x,t)
  global kg ad ec pp fr h0 vol;
  pm = 1; % This is how much to make the g-model different from f (two curves)
  % Maybe this should a be noise source instead of a fixed offset?
  lkg = delta(kg, -0.02*pm); % Local (varied) normal E. coli doubling time in minutes
  lad = delta(ad, -0.1*pm);  % Local (varied) adsorption coefficient mL/min
  lec = delta(ec, -0.1*pm);  % Local (varied) eclipe interval  (6 min?)
  lpp = delta(pp, -0.05*pm);  % Local (varied) phage production per cell
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

% fr is flow rate in volumes per hour:  fr(vol/hour)*vol(mL/vol)/60(min/hour) is flow rate in mL/min   
% h0 is input concentration in cells/mL:   h0(cells/mL)  *  fr*vol/60(mL/min) is cells/min
%    
frpm = h0*fr*vol/60 + (log(2)/lkg)*x(1) - dc*x(1)  - (lad*60*x(1)*x(4))/vol;

% Adsorption constant is in mL/min (physical interpretation: Brownian motion. Think of mL as cubic-centimeter)
% lad*x(1)*x(4)     (mL/min)*(cells)*(total phage)  (interaction-volume/min)*(targets)*(missiles)
%
% Note uninfected host can become quite scarce, while phage become numerous,
% reducing the chance that a given phage will find a target.
% This is the crutial point for an emerging population.
% The total number of host cells (collision opportunities for phage) is x1+x2*x3
% while only x1 collisions can result in an infection, so probability of collision is: x1/(x1+x2+x3)
%
% (1) Uninfected Host flowing in at Dilution Constant * Cells/mL
%           + INFLOW      +    GROWTH         - OUTFLOW    - TRANSFORMATION
%  xdot(1) =   h0*fr*vol/60  + (log(2)/lkg)*x(1) - fr*x(1)/60 - (lad/vol)*x(4)*(x(1)/(x(1)+x(2)+x(3)));
  xdot(1) =   h0*fr*vol/60  + (log(2)/lkg)*x(1) - fr*x(1)/60 - (lad/vol)*x(4)*x(1);

% (2) Infected Host (previous TRANSFORMED is this equation's INFLOW)
%          (mL/min) * phage/vol * (cells/vol)/(cells/vol)
%  xdot(2) =  (lad/vol)*x(4)*(x(1)/(x(1)+x(2)+x(3))) + (log(2)/kgp)*x(2) - fr*x(2)/60  -  x(2)/lec
   xdot(2) =  (lad/vol)*x(4)*x(1) + (log(2)/kgp)*x(2) - fr*x(2)/60  -  x(2)/lec;

% (3) Productive Host: INFLOW from Eclipse state, plus growth, minus dilution 

  xdot(3) =  x(2)/lec   + (log(2)/kgpp)*x(3)  - fr*x(3)/60;  % No transformation output
  
% (4) Phage Production:  No Growth      - OUTFLOW   % No transformation output

  xdot(4) =  lpp*x(3)/60                - fr*x(4)/60;
  
endfunction


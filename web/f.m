function xdot = f(x,t,kon)
global k1 k2 k3 k4 k5 k6 kg fr h0 vol;

% fr is volumes/hour
% vol is mL/volume
% ODE Steps are minutes
% Dilution constant =
%    FlowRate  *  Volume    * ODE Stepsize = dc
%    (Vol/Hour)*(ml/Volume) * Hours/Step   = mL/step
  dc = (fr*vol)/60;   % Flow rate in mL per minute
  p = 0.4;
% Each successive growth rate (slightly) inhibited
  lk1 = delta(k1,-0.1);
  lk2 = delta(k2,-0.1);
  lk3 = delta(k3,-0.05);
  lk4 = delta(k4,-0.1);
  lkg = delta(kg,-0.1);

  inhibit = 0.9;
  kgp = kg*inhibit;
  kgpp = kgp*inhibit;
  kgppp = kgpp*inhibit;

% Uninfected Host flowing in at Dilution Constant * Cells/mL
% Inflow - outflow  - infected cells + normal ecoli growth
  xdot(1) = dc*h0 - dc*x(1) - lk1*x(1)*x(4) + lkg*x(1);
% Infected Host
  xdot(2) = -dc*x(2) + lk1*x(1)*x(4) + kgp*x(2) - lk2*x(2);
% Productive Host
%   minus dilution, plus growth, plus cells leaving eclise  
  xdot(3) = -dc*x(3) + kgpp*x(3) + lk2*x(2);
% Phage    minus outflow plus phage production
  xdot(4) = -dc*x(4) + kgppp*x(4) + lk4*x(3);
% Extinct (washed out) Host  (this graph not labeled)
  xdot(5) = -dc*x(5) - lk1*p*x(1) + lk3*x(3);
  endfunction


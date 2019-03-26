pkg load image
hostcolor =  [0.00 0.00 1.00];  % Blue Devil Blue
host2color = [0.00 0.75 0.00];  % Green
host3color = [0.75 0.00 0.00];  % Red
phagecolor = [0.00 0.40 0.80];  % Carolina Blue
zcolor     = [0.75 0.00 0.75];  % Purple
% colors = [hostcolor, host2color, host3color, phagecolor]

global kg ad dur ec pp p0 fr h0 vol dur;

% ad  = Adsorption Factor               mL/min
% kg  = Ecoli Growth Rate               doubling time in minutes
% pp  = per host cell phage production  phage/cell-hour
% fr  = Flow Rate                       volumes/hour 
% vol = Total volume of culture in      mL
% lsl = Linear or Semi-log graph 1=linear 0=semilog

load ES_params.txt;  % Loads the global variables
hours = dur;
steps = hours*60;

%  Initial conditions:
% HOST INNOCULATION: x(1)=1000
% PHAGE INFECTION:   x(4)=10^9
% and x(2..3) = 0 on [0,20] with 200 points:
x = lsode("f2", [h0*vol;0;0;p0], (t = linspace (0,hours,steps)' ));
y = lsode("g2", [h0*vol;0;0;p0], (u = linspace (0,hours,steps)' ));
fhandle = figure(1);
if (lsl == 1)
  plot(t,x);
  hold on;
  plot(t,y);
else
  semilogy(t,x);
  hold on;
  semilogy(t,y);
endif

[fh,msg] = jpfill(t,x(:,1), y(:,1),hostcolor);
[fh,msg] = jpfill(t,x(:,2), y(:,2),host2color);
[fh,msg] = jpfill(t,x(:,3), y(:,3),host3color);
[fh,msg] = jpfill(t,x(:,4), y(:,4),phagecolor);

middle = steps/2;
leftp = middle/3;
to = x(middle,1)*0.05;  % tiny offset
so = x(middle,1)*0.1;  % small offset
o = x(middle,1)*0.2;
lo = x(middle,1)*0.4;  % large offset

pos1 = dur/2;
pos2 = dur/6;
lp = steps - 10;
rx = dur + dur*0.01;
% Y position is the data axis, so to get nice label offset from curves:
% Call to offset does the following:
% Multiply by 10^2 for semilog, add/subtract fraction for linear.
text(pos1,
     offset(x(middle,1),lsl,x(middle,4)),
     'Uninfected Host','fontsize',20,'color',hostcolor);
righty = rv(lp,x, y, 1, lsl, 0);
text(rx, righty, 'Uninfected','fontsize',10,'color',hostcolor);
righty = rv(lp,x, y, 1, lsl, -0.3);
text(rx, righty, 'Host','fontsize',10,'color',hostcolor);
disp("after uninfected");
text(pos2,
     offset(x(leftp,2),lsl,x(leftp,3)),
     'Adsorbed Host',  'fontsize',20,'color',host2color);
righty = rv(lp,x, y, 2, lsl, 0.3);
text(rx, righty, 'Adsorbed','fontsize',10,'color',host2color);
righty = rv(lp,x, y, 2, lsl, 0);
text(rx, righty,   'Host','fontsize',10,'color',host2color);
disp("after adsorbed");

text(pos2,
     offset(x(leftp,3),lsl,x(leftp,2)),
     'Productive Host','fontsize',18,'color',host3color);
righty = rv(lp,x, y, 3, lsl, 0.4);
text(rx, righty, 'Productive','fontsize',10,'color',host3color);
righty = rv(lp,x, y, 3, lsl, 0.1);
text(rx, righty, 'Host','fontsize',10,'color',host3color);
disp("after productive");


text(pos1,
     offset(y(leftp,4),lsl,x(leftp,2)),
     'Phage',        'fontsize',32,'color',phagecolor);
righty = rv(lp, x, y, 4, lsl, 0);
text(rx, righty, 'Phage','fontsize',10,'color',phagecolor);
disp("after Phage");

print(fhandle,'-dpng','-color','phagepop.png');


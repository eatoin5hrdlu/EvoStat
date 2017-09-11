pkg load image
hostcolor =  [0.00 0.00 1.00];  % Blue Devil Blue
host2color = [0.00 0.75 0.00];  % Green
host3color = [0.75 0.00 0.00];  % Red
phagecolor = [0.20 0.25 0.65];  % Carolina Blue
zcolor     = [0.75 0.00 0.75];  % Purple
% colors = [hostcolor, host2color, host3color, phagecolor]

global kg ad dur ec pp p0 fr h0 vol dur;

% ad  = Adsorption Factor               1/hour
% kg  = Ecoli Growth Factor             1/hour
% pp  = per host cell phage production  phage/cell-hour
% fr  = Flow Rate                       volumes/hour    
% vol = Total volume of culture in      mL
% lsl = Linear or Semi-log graph 1=linear 0=semilog

load web/ES_params.txt;  % Loads the global variables
hours = dur;
steps = hours*60;

%  Initial conditions:
% HOST INNOCULATION: x(1)=1000
% PHAGE INFECTION:   x(4)=10^9
% and x(2..3) = 0 on [0,20] with 200 points:
x = lsode("f", [1000;0;0;p0], (t = linspace (0,hours,steps)' ));
y = lsode("g", [1000;0;0;p0], (u = linspace (0,hours,steps)' ));
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
leftp = middle/2;
to = x(middle,1)*0.05;  % tiny offset
so = x(middle,1)*0.1;  % small offset
o = x(middle,1)*0.2;
lo = x(middle,1)*0.4;  % large offset

pos1 = dur/2;
pos2 = dur/5;
text(pos1,x(middle,1)-lo,'Uninfected Host','fontsize',22,'color','k');
text(pos2,x(leftp,2)+so,  'Adsorbed Host',  'fontsize',24,'color',host2color);
text(pos2,x(leftp,3)+to,  'Productive Host','fontsize',20,'color',host3color);
text(pos1,x(middle,4),   'Phage',          'fontsize',32,'color',phagecolor);

print(fhandle,'-dpng','-color','web/phagepop.png');


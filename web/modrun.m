pkg load image
hostcolor =  [0.00 0.00 1.00];  % Blue Devil Blue
host2color = [0.00 0.75 0.00];  % Green
host3color = [0.75 0.00 0.00];  % Red
phagecolor = [0.75 0.00 0.75];  % Purple
zcolor =     [0.00 0.25 0.75];  % Carolina Blue

global k1 k2 k3 k4 k5 k6 kg fr h0 vol;
load web/ES_params.txt;  % Loads the global variables
hours = 20;
steps = hours*60;

%  Initial conditions x(1)=1000 and x(2..5)=0 on [0,20] with 200 points:
x = lsode("f", [1000;0;0;0;0], (t = linspace (0,hours,steps)' ));
y = lsode("g", [1000;0;0;0;0], (u = linspace (0,hours,steps)' ));
fhandle = figure(1);
plot(t,x);
hold on;
plot(t,y);

[fh,msg] = jpfill(t,x(:,1), y(:,1),hostcolor);
[fh,msg] = jpfill(t,x(:,2), y(:,2),host2color);
[fh,msg] = jpfill(t,x(:,3), y(:,3),host3color);
[fh,msg] = jpfill(t,x(:,4), y(:,4),zcolor);
[fh,msg] = jpfill(t,x(:,5), y(:,5),phagecolor);

to = x(200,1)*0.05;  % tiny offset
so = x(200,1)*0.1;  % small offset
o = x(200,1)*0.2;
lo = x(200,1)*0.3;  % large offset

text(10,x(200,1)+lo,'Uninfected Host','fontsize',22,'color',hostcolor);
text(10,x(200,2)+so,'Adsorbed Host','fontsize',24,'color',host2color);
text( 8,x(200,3)+to,  'Productive Host','fontsize',20,'color',host3color);
% text(10,x(400,4)-so,'Z','fontsize',28,'color',zcolor);
text(10,x(200,5),'Phage','fontsize',32,'color','b');

print(fhandle,'-dpng','-color','web/phagepop.png');


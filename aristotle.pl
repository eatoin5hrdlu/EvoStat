config( [
	 textMessages(3600),  % Twice Daily (6min=360 hour=3600 4-hour=14400)
	 updateCycle(90),    % In seconds
	 debugpause(10),      % Debug essentially off when pause is 10ms
	 numLagoons(2),
         imageSize(580,440),
%        cellstatRegion(ytop,xleft,ybot,xright),
         cellstatRegion(130,210,360,260),
	 cellstatContrast(1, 1.03, -100), % Iterations, Multiply, Subtract
	 cellstatThreshold(144),
	 cellstatHeight(230),  % same as 100% of cellstat volume

         lagoonRegion(470,10,620,460),
         lagoonContrast(  2, 3.5, -50),
	 lagoonThreshold(110),
	 lagoonHeight(130),          % same as 100% of lagoon volume
	 lagoonWidth(40),

	 hostReticule(200,300,400,450),
	 hostReticuleContrast(1, 1.4, -100),
	 hostReticuleThreshold(144),

	 frames(100),       % number of frames for lumosity integration
	 darkness(60),      % Average pixel threshold to identify darkness
	 camera(0),
	 rotate(0),
	 screen(40,52,point(120,1)),
	 layout([
		 supply( nutrient, below,  [Supply,levelUnits('L'),v(10)]),
		 supply( arabinose, right, [Supply,levelUnits(mL)]),
		 supply( inducer2,  right, [Supply,levelUnits(mL)]),
		 supply( inducer3,  right, [Supply,levelUnits(mL)]),
		 cellstat( host0,   below, [tb(400),shape(36,14),CF]),
		 spacer(     x1, next_row, [color(blue)]),
		 snapshot(  cam, next_row, [ ]),
		 spacer(      x2, next_row, []),
		 lagoon( lagoon1, next_row, [TL, LS, LF]),
		 lagoon( lagoon2, right,    [TL, LS, LF]),
		 lagoon( lagoon3, right,   [TL, LS, LF]),
		 spacer(      x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [shape(40,10),up(45),SF])
                ])
	 ]) :-
 Supply = shape(11,4),
 LS = shape(31,14),
 TL = tl(31),
 LF = font(font(times,roman,18)),
 CF = font(font(times,roman,16)),
 SF = font(font(times,roman,18)).

% When testing with no devices, uncomment next line for fast startup.
% bt_address(Name, Addr) :- !, fail.

%bt_device(nutrient,   '98:D3:31:30:2A:D1').
% bt_device( cellstat, '98:D3:31:50:12:F4'). % HC-06 could be bad now
bt_device(   host0, '98:D3:31:40:1E:80'). % HC-06 could be bad now
%NEW bt_device(newcellstat, '98:D3:31:FD:2D:05'). % HC-06 could be bad now
%bt_device(lagoon1,    '98:D3:31:80:34:39').

%bt_device(lagoon1,     '98:D3:31:50:14:06').
bt_device( lagoon2,     '98:D3:31:70:2B:70').
bt_device(autosampler, '98:D3:31:40:1D:D4').

%bt_device(cellstat,    '98:D3:31:70:3B:34'). % Lagoon1 substituted
%bt_device(cellstat,    '98:D3:31:90:29:0E').
%bt_device( lagoond2,   '98:D3:31:30:95:60').
%bt_device( lagoon3,    '98:D3:31:70:3B:2B').

%bt_device(labcellstat,  '98:D3:31:90:29:0E').
%bt_device(cellstat,     '98:D3:31:90:29:0E').
%bt_device(  lagoond3,   '98:D3:31:80:34:39'). % was d3
%bt_device(autosampler,  '98:D3:31:40:1D:D4').

%bt_device(autosamplerY, '98:D3:31:20:23:4F').

% BETA BOX simulator 
%bt_device( cellstat,     '98:D3:31:90:2B:82').
%bt_device(  lagoon1,     '98:D3:31:70:2A:22').
%bt_device(  lagoon2,     '98:D3:31:40:31:BA').
%bt_device(  autosampler, '98:D3:31:20:2B:EB').

% Museum simulator
%bt_device( cellstatd,     '98:D3:31:40:90:13').
%bt_device(  lagoond1,     '98:D3:32:30:42:6A').
%swbt_device(  lagoond3,     '98:D3:31:80:34:39').
%bt_device(autosampler,    '98:D3:31:30:95:4B').

% Recipients of texts ( vp = verizon(picture), a = AT&T )

% watcher (Name,  '<carrier> <number>', Hours-per-text)

watcher(reintjes,'vp 9194525098',  4).  % Peter Reintjes
%watcher(laurie,  'vp 9196987470', 24).   % Laurie Betts
%watcher(pc,      'a 9193083839',  8).  % The Other Peter
%watcher(marshall,'a 5056037415', 8).   % Marshall
%watcher(martha, 'vp 9196024293', 23).  % Martha Collier
%watcher(lea,    'vp 9194525097', 4).   % Lea
%watcher(howell, 'vp 7723215578', 48).  % Finn Howell

% Fake Level Data for PID debugging
% simulator.
% input(lagoon1, 41).

% pid(Component,
%     Kp, Ki, Kd, 
%     Variable, TargetValue,
%     Minimum, Maximum, SampleTime)
% Undershoot/Overshoot: Modify Kd and maybe Kp
% Response too slow:    Increase Ki (Kp?)

deadzone(l,2).  % No change needed if param is this close
deadzone(t,1).  % Temperature closer than level  

% l = level
% t = temperature    
pid_controllers([
   pid(   host0, 1.0, 0.0, 0.1, l, 71, 10, 100, 120),
   pid( lagoon1, 1.0, 0.0, 0.1, l, 31, 10, 100, 120)]).

% control(Component, Param, Pos-Ctrl, Alt Component, Neg-Ctrl)
% For example:    
%  control(Component, level, InflowTime, Alt-Component, OutflowTime)
component_valves(   host0,  v0, autosampler, v0).
component_valves( lagoon1,  v1, autosampler, v1).
component_valves( lagoon2,  v1, autosampler, v2).
component_valves( lagoon3,  v1, autosampler, v3).
component_valves( lagoon4,  v1, autosampler, v4).

    
%%%%%%%%%%%%%% SYSTEM/USER DEPENDENT STUFF 

% To build stand-alone executable there are different emulators
:- discontiguous python/1.

python('C:\\Python27\\python.exe') :- windows.
python('/usr/bin/python')          :- linux.


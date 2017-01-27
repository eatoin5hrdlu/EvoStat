config( [
	 textMessages(3600),  % Twice Daily (6min=360 hour=3600 4-hour=14400)
	 updateCycle(30),    % In seconds
	 debugpause(10),      % Debug essentially off when pause is 10ms
	 numLagoons(1),
         imageSize(580,440),

         cellstatRegion(180,220,360,270),
%         cellstatRegion(205,250,400,310),
         cellstatContrast(2, 1.3, -70), % Iterations, Multiply, Subtract
	 cellstatHeight(230),  % same as 100% of cellstat volume

         lagoonRegion(470,10,620,460),
         lagoonContrast(  3, 2.8, -30),
	 lagoonHeight(130),          % same as 100% of lagoon volume
	 lagoonWidth(40),

	 frames(100),       % number of frames for lumosity integration
	 darkness(60),      % Average pixel threshold to identify darkness
	 camera(0),
	 rotate(90),
	 screen(40,52,point(120,1)),
	 layout([
		 supply( nutrient, below,  [Supply,levelUnits('L')]),
		 supply( arabinose, right, [Supply,levelUnits(mL)]),
		 supply( inducer2,  right, [Supply,levelUnits(mL)]),
		 supply( inducer3,  right, [Supply,levelUnits(mL)]),
		 cellstat(newcellstat,below, [tb(400),tt(370),shape(36,12),CF]),
		 spacer(     x1, next_row, [color(blue)]),
		 snapshot(  cam, next_row, [ ]),
		 spacer(      x2, next_row, []),
		 lagoon( lagoon1, next_row, [tt(350), TL, LS, LF]),
		 lagoon( lagoond2, right,    [tt(350), TL, LS, LF]),
		 lagoon( lagoon3, right,   [tt(345) , TL, LS, LF]),
		 spacer(      x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [shape(40,12),SF])
                ])
	 ]) :-
 Supply = shape(10,5),
 LS = shape(31,12),
 TL = tl(31),
 LF = font(font(times,roman,20)),
 CF = font(font(times,roman,18)),
 SF = font(font(times,roman,20)).

% When testing with no devices, uncomment next line for fast startup.
% bt_address(Name, Addr) :- !, fail.

bt_device(nutrient,    '98:D3:31:30:2A:D1').
bt_device(lagoon1,    '98:D3:31:80:34:39').
bt_device(autosampler, '98:D3:31:40:1D:D4').
    
%current bt_device(cellstat,    '98:D3:31:50:12:F4'). % HC-06

%bt_device(cellstat,    '98:D3:31:70:3B:34'). % Lagoon1 substituted
%bt_device(cellstat,    '98:D3:31:90:29:0E').
%bt_device( lagoond2,   '98:D3:31:30:95:60').
%bt_device( lagoon3,    '98:D3:31:70:3B:2B').

%bt_device(labcellstat,  '98:D3:31:90:29:0E').
%bt_device(cellstat,     '98:D3:31:90:29:0E').
%bt_device(  lagoond3,   '98:D3:31:80:34:39'). % was d3
%bt_device(autosampler,  '98:D3:31:40:1D:D4').

%bt_device(autosamplerY, '98:D3:31:20:23:4F').
%bt_device(autosamplerZ, '98:D3:31:70:2B:70').

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

watcher(reintjes,'vp 9194525098', 12).  % Peter Reintjes
watcher(laurie,  'vp 9196987470', 24).   % Laurie Betts
%watcher(pc,      'a 9193083839',  8).  % The Other Peter
%watcher(marshall,'a 5056037415', 8).   % Marshall
%watcher(martha, 'vp 9196024293', 23).  % Martha Collier
%watcher(lea,    'vp 9194525097', 4).   % Lea
%watcher(howell, 'vp 7723215578', 48).  % Finn Howell

% Fake Level Data for PID debugging
simulator.
input(lagoon1, 41).

% pid(Component,
%     Kp, Ki, Kd, Polarity,
%     TargetValue, CurrentValue,
%     Minimum, Maximum, SampleTime)
% Undershoot/Overshoot: Modify Kd and maybe Kp
% Response too slow:    Increase Ki

pid_controllers([
   pid(cellstat,0.4, 0.3, 0.3, 85, 10, 100, 30),
   pid(lagoon1, 0.4, 0.3, 0.3, 30, 10, 100, 30)]).

% control(Component, Param, Pos-Ctrl, Alt Component, Neg-Ctrl)
% For example:    
%  control(Component, level, InflowTime, Alt-Component, OutflowTime)
control( cellstat, level, 'v0', autosampler, 'v0').
control(  lagoon1, level, 'v1', autosampler, 'v1').
control(  lagoon2, level, 'v1', autosampler, 'v2').
control(  lagoon3, level, 'v1', autosampler, 'v3').
control(  lagoon4, level, 'v1', autosampler, 'v4').

%%%%%%%%%%%%%% SYSTEM/USER DEPENDENT STUFF 

% To build stand-alone executable there are different emulators
:- discontiguous python/1.

python('C:\\Python27\\python.exe') :- windows.
python('/usr/bin/python')          :- linux.


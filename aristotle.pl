config( [
	 textMessages(14400), % Twice Daily (10min=360 hour=3600 4-hour=14400)
	 updateCycle(90),    % In seconds
	 debugpause(10),      % Debug essentially off when pause is 10ms
	 numLagoons(1),
         imageSize(580,440),

         cellstatRegion(120,220,500,260),
         cellstatContrast(1, 2.0, -40), % Iterations, Multiply, Subtract
	 cellstatHeight(250),  % same as 100% of cellstat volume

         lagoonRegion(400,20,630,460),
         lagoonContrast(  2, 1.9, -69),
	 lagoonHeight(120),             % same as 100% of lagoon volume
	 lagoonWidth(60),

	 frames(100),       % number of frames for lumosity integration
	 darkness(60),      % Average pixel threshold to identify darkness
	 camera(0),
	 rotate(90),
	 screen(46,48,point(720,1)),
	 layout([
		 cellstat(cellstat,below,[od(0.4),temp(37.0),shape(36,12),CF]),
		 spacer(     x1, next_row, [color(blue)]),
		 snapshot(  cam, next_row, [ image('mypic1.jpg'),shape(42,42)]),
		 spacer(      x2, next_row, []),
		 lagoon( lagoon1, next_row, [temp(35.0), LS, LF]),
		 lagoon( lagoon2, right,    [temp(35.0), LS, LF]),
		 lagoon( lagoon3, right,   [temp(34.5) ,LS, LF]),
		 spacer(      x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [shape(40,12),SF])
                ])
	 ]) :-
 LS = shape(31,12),
 LF = font(font(times,roman,20)),
 CF = font(font(times,roman,18)),
 SF = font(font(times,roman,20)).

% When testing with no devices, uncomment next line for fast startup.
% bt_address(Name, Addr) :- !, fail.

bt_device(cellstat,    '98:D3:31:90:29:0E').
bt_device( lagoon1,    '98:D3:31:80:34:39'). % was d3
bt_device(autosampler, '98:D3:31:40:1D:D4').

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
%bt_device(  lagoond2,     '98:D3:31:30:95:60').
%swbt_device(  lagoond3,     '98:D3:31:80:34:39').
%bt_device(autosampler,    '98:D3:31:30:95:4B').

% Recipients of texts ( vp = verizon(picture), a = AT&T )

%watcher('vp 9194525097'). % Lea
watcher('vp 9194525098'). % Peter Reintjes
watcher('a 9193083839').  % The Other Peter
watcher('a 5056037415').  % Marshall

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

%control(Component, Param, Pos-Ctrl, Alt Component, Neg-Ctrl)
control(cellstat, level, 'v0', autosampler, 'm').
control(  lagoon1, level, 'v1', autosampler, 'i').

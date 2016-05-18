config( [
	 textMessages(14400), % 10 min = 360 hourly = 3600 4-hours = 14400
	 updateCycle(90),
	 debugpause(10),

         cellstatRegion(80,120,300,350),
         cellstatContrast(3,1.5,-70),
	 cellstatHeight(220),   % This sets 100% vessel full line

         lagoonRegion(330,20,510,470),
         lagoonContrast(2,1.4,-70),
	 lagoonHeight(180),

	 numLagoons(3),

	 lagoonWidth(20),
	 levelScale(100),               % level as percentage of lagoonHeight
	 frames(100),                    % # frames to integrate for luminosity
	 camera(0),
	 rotate(false),
	 screen(42, 93, point(500,20)), % W,H are percentage of computer screen
         imageSize(580,440),            % size of snapshot in pixels
	 layout([
		 supply( nutrient, below,  [SupplyShape,level(20)]),
		 supply( arabinose, right, [SupplyShape,level(500),levelUnits(mL)]),
		 supply( inducer3,  right, [SupplyShape,level(200),levelUnits(mL)]),
		 supply( inducer2,  right, [SupplyShape,level(10),levelUnits(mL)]),
		 cellstat(cellstatb,below,[od(0.4),temp(37.0),levelUnits('%'), shape(30,10),CF]),
		 spacer(        x1, next_row, [color(blue)]),
		 snapshot(     cam, next_row, [ image('mypic1.jpg'),shape(40,30)]),
		 spacer(        x2, next_row, []),
		 lagoon( lagoon1, next_row, [temp(36.8), LU, LS, SF]),
		 lagoon( lagoon2, right,    [temp(35.0), LU, LS, SF]),
		 lagoon( lagoon3, right,    [temp(34.5) ,LU, LS, SF]),
		 spacer(        x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [flow(2.3), shape(40,8),SF]),
		 drainage(waste, next_row, [shape(20,9),SF])
                ])
	 ]) :-
 SupplyShape = shape(10,5),

 LU = levelUnits(mL),
 LS = shape(30,11),
% LF = font(font(times,roman,14)),
 CF = font(font(times,roman,16)),
 SF = font(font(times,roman,14)).

% BETABOX simulator

%bt_device(_,_) :- !, fail.
bt_device(cellstatb,     '98:D3:31:90:2B:82').
%bt_device(  lagoon1,     '98:D3:31:70:3B:34').
bt_device(  lagoon1,     '98:D3:31:70:2A:22').
bt_device(  lagoon2,     '98:D3:31:40:31:BA').
bt_device(  autosampler, '98:D3:31:20:2B:EB').

simulator.
deadzone(2).

input(cellstatb,82).
input(lagoon1, 30).
input(lagoon2, 40).
input(lagoon3, 34).
input(lagoon4, 38).

% logfile(logfile).
%watcher('vp 9194525097'). % Lea
watcher('vp 9194525098'). % Peter
%watcher('a 9193083839'). % The Other Peter
% pid(Component,Kp,Ki,Kd,Target,Min,Max,DeltaT)

pid_controllers([
   pid(cellstatb,0.4,0.3, 0.3, 85, 10, 100, 30),
   pid(lagoon1, 0.4, 0.3, 0.3, 30, 10, 100, 20),
   pid(lagoon2, 0.4, 0.3, 0.3, 30, 10, 100, 10),
   pid(lagoon3, 0.4, 0.3, 0.3, 30, 10, 100, 7),
   pid(lagoon4, 0.4, 0.3, 0.3, 30, 10, 100, 9)]).

control(cellstatb, level, 'v0', autosampler, 'm').
control(lagoon1,  level, 'v1', autosampler, 'i').
control(lagoon2,  level, 'v1', autosampler, 'j').
control(lagoon3,  level, 'v1', autosampler, 'k').
control(lagoon4,  level, 'v1', autosampler, 'k').


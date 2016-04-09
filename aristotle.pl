config( [
	 updateCycle(60),  % In seconds
	 debugpause(10),
	 numLagoons(1),
         imageSize(580,440),

         cellstatRegion(10,200,290,330),
         cellstatContrast(1, 1.2, -90), % Iterations, Multiply, Subtract
	 cellstatHeight(250),  % same as 100% of cellstat volume

         lagoonRegion(400,20,620,460),
         lagoonContrast(  2, 1.9, -69),
	 lagoonHeight(190),    % same as 100% of lagoon volume
	 lagoonWidth(60),

	 frames(100),       % number of frames for lumosity integration
	 darkness(60),      % Average pixel threshold to identify darkness
	 rotate(90),
	 camera(0),
	 screen(46,48,point(720,1)),
	 layout([
		 cellstat(cellstat,below,[od(0.4),temp(37.0),shape(36,12),CF]),
		 spacer(     x1, next_row, [color(blue)]),
		 snapshot(  cam, next_row, [ image('mypic1.jpg'),shape(42,42)]),
		 spacer(      x2, next_row, []),
		 lagoon( lagoon1, next_row, [temp(35.0), LS, SF]),
		 lagoon( lagoon2, right,    [temp(35.0), LS, SF]),
		 lagoon( lagoon3, right,   [temp(34.5) ,LS, SF]),
		 spacer(      x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [shape(40,10),SF])
                ])
	 ]) :-
 LS = shape(31,12),
% LF = font(font(times,roman,14)),
 CF = font(font(times,roman,18)),
 SF = font(font(times,roman,20)).


bt_device(cellstat,    '98:D3:31:90:29:0E').
bt_device( lagoon1,    '98:D3:31:80:34:39'). % was d3
bt_device(autosampler, '98:D3:31:40:1D:D4').

% But Linux can be flaky too, so bt_device(Name,Address) facts will
% be in <hostname>.pl (or <evostat>.pl) with config/1.

% When no devices are around, uncomment the following for fast startup.
% bt_address(Name, Addr) :- !, fail.

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

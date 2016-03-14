config( [
	 numLagoons(1),
         cellstatRegion(90,250,300,290),
         lagoonRegion(370,40,530,450),
	 lagoonHeight(130),    % divisor for levelScale
	 lagoonWidth(60),
	 levelScale(140),   % Maximum percentage or mL
	 levelOffset(10),   % Minimum percentage or mL
	 frames(100),       % number of frames for lumosity integration
	 darkness(60),      % Average pixel threshold to identify darkness
	 camera(outdoor),
	 rotate(90),
	 mac(0),  % belongs in snapshot
	 defaultIP('172.16.3.136'),  % belongs in snapshot
	 userpwd('&user=scrapsec&pwd=lakewould'),
	 brightness(11), % 0-240 for indoor camera
	 brightnessCmd('/camera_control.cgi?param=1&value='),
	 contrast(40),
	 contrastCmd('/camera_control.cgi?param=2&value='),
	 picCmd('/snapshot.cgi?resolution=32&user=admin&pwd=lakewould'),
	 screen(60,90,point(200,1)),
	 layout([
		 cellstat(cellstat,below,[od(0.4),temp(37.0),shape(20,6),CF]),
		 spacer(        x1, next_row, [color(blue)]),
		 snapshot(     cam, next_row, [ image('mypic1.jpg'),shape(30,30)]),
		 spacer(        x2, next_row, []),
		 lagoon( lagoon1, next_row, [temp(35.0), LS, SF]),
		 lagoon( lagoon2, right,    [temp(35.0), LS, SF]),
		 lagoon( lagoon3, right,    [temp(34.5) ,LS, SF]),
		 spacer(        x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [shape(40,5),SF])
                ])
	 ]) :-
 LS = shape(24,5),
% LF = font(font(times,roman,14)),
 CF = font(font(times,roman,18)),
 SF = font(font(times,roman,20)).

% bt_device(labcellstat,   '98:D3:31:90:29:0E').
% bt_device(cellstat,     '98:D3:31:90:29:0E').
% bt_device(autosampler,  '98:D3:31:40:1D:D4').
%           '98:D3:31:20:23:4F',
%           '98:D3:31:70:2B:70',

% These are the simulated devices in the Arduino Mega2560 box
bt_device( cellstat,     '98:D3:31:90:2B:82').
bt_device(  lagoon1,     '98:D3:31:70:2A:22').
bt_device(  lagoon2,     '98:D3:31:40:31:BA').
bt_device(  lagoon3,     '98:D3:31:20:2B:EB').

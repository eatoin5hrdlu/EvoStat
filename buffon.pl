
config( [
	 numLagoons(3),
         imageSize(600,500),
%         lagoonRegion(250,50,400,520),
         lagoonRegion(240,36,440,575),
         cellstatRegion(80,200,240,400),
	 lagoonHeight(160),    % divisor for levelScale
	 lagoonWidth(80),
	 levelScale(140),   % Maximum percentage or mL
	 levelOffset(10),   % Minimum percentage or mL
	 frames(100),       % number of frames for lumosity integration
	 darkness(60),      % Average pixel threshold to identify darkness
	 camera(outdoor),
	 rotate(false),
	 mac(1),  % belongs in snapshot
	 defaultIP('172.16.3.136'),  % belongs in snapshot
	 userpwd('&user=scrapsec&pwd=lakewould'),
	 brightness(11), % 0-240 for indoor camera
	 brightnessCmd('/camera_control.cgi?param=1&value='),
	 contrast(40),
	 contrastCmd('/camera_control.cgi?param=2&value='),
	 picCmd('/snapshot.cgi?resolution=32&user=admin&pwd=lakewould'),
	 screen(680, 870, point(750,0)),
	 layout([
		 cellstat(cellstat,below,[od(0.4),temp(37.0),shape(240,80),mac('98:D3:31:20:23:4f'),CF]),
		 % pumps( pumprail, next_row,   [  mac('98:D3:31:70:2B:70')]),
%		 pumps( pumprail, next_row,   [  ]),
		 spacer(        x1, next_row, [color(blue)]),
		 snapshot(     cam, next_row, [ shape(340,420),image('mypic1.jpg')]),
		 spacer(        x2, next_row, []),
		 lagoon( lagoon1, next_row, [temp(35.0), LS, SF]),
		 lagoon( lagoon2, right,    [temp(35.0), mac('98:D3:31:70:3B:34'),LS, SF]),
%		 lagoon( lagoon3, right,    [temp(34.5), mac('98:D3:31:20:23:36'),LS, SF]),
		 lagoon( lagoon3, right,    [temp(34.5),LS, SF]),
%		 lagoon( lagoon4, right,    [temp(35.0), LS, LF]),

		 spacer(        x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [ shape(400,30),SF])
                ])
	 ]) :-
 LS = shape(180,78),
% LF = font(font(times,roman,14)),
 CF = font(font(times,roman,18)),
 SF = font(font(times,roman,20)).


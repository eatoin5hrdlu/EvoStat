config( [
	 updateCycle(60),  % In seconds
	 debugpause(100),
	 numLagoons(1),
         imageSize(630,500),

         cellstatRegion(10,240,300,290),
	 cellstatScale(1500),  % Maximum mL
	 cellstatOffset(100),  % Minimum mL

         lagoonRegion(410,20,620,460),
	 lagoonHeight(170),    % divisor for levelScale
	 lagoonWidth(60),
	 lagoonScale(140),   % Maximum percentage or mL
	 lagoonOffset(10),   % Minimum percentage or mL

	 frames(100),       % number of frames for lumosity integration
	 darkness(60),      % Average pixel threshold to identify darkness
	 camera(outdoor),
	 rotate(90),
	 mac(0),  % belongs in snapshot
	 defaultIP('172.16.3.136'),  % belongs in snapshot
	 userpwd('&user=scrapsec&pwd=lakewould'),
	 brightness(11), % 0-240 for indoor camera
         brightnessCmd('/camera_control.cgi?param=1&value='),
         cellstatContrast(3, 1.35, -71), % Iterations, Multiply, Subtract
         lagoonContrast(  3, 1.62, -69),
	 contrast(40),
	 contrastCmd('/camera_control.cgi?param=2&value='),
	 picCmd('/snapshot.cgi?resolution=32&user=admin&pwd=lakewould'),
	 screen(46,48,point(720,1)),
	 layout([
		 cellstat(cellstat,below,[od(0.4),temp(37.0),shape(36,12),CF]),
		 spacer(     x1, next_row, [color(blue)]),
		 snapshot(  cam, next_row, [ image('mypic1.jpg'),shape(42,42)]),
		 spacer(      x2, next_row, []),
		 lagoon( lagoon1, next_row, [temp(35.0), LS, SF]),
		 lagoon( lagoon2, right,    [temp(35.0), LS, SF]),
		 lagoon( lagoond3, right,   [temp(34.5) ,LS, SF]),
		 spacer(      x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [shape(40,10),SF])
                ])
	 ]) :-
 LS = shape(31,12),
% LF = font(font(times,roman,14)),
 CF = font(font(times,roman,18)),
 SF = font(font(times,roman,20)).


bt_device(cellstat,     '98:D3:31:90:29:0E').
bt_device(  lagoond3,   '98:D3:31:80:34:39'). % was d3
bt_device(autosampler,  '98:D3:31:40:1D:D4').

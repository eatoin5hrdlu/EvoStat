config( [
	 updateCycle(60),  % In seconds
	 debugpause(10),
	 numLagoons(1),
         imageSize(630,500),

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

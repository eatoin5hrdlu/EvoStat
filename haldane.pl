config( [
	 numLagoons(3),
	 cellstatRegion(30,100,200,350),
%        lagoonRegion(427,4,584,473),
%        lagoonRegion(47,458,607,324),
%        lagoonRegion(458,47,324,607),
%        lagoonRegion(324,47,458,607),
         lagoonRegion(264,45,468,637),
	 lagoonHeight(120),    % divisor for levelScale
	 lagoonWidth(60),
	 levelScale(140),
	 levelOffset(10),
	 levelScale(100),   % 100 gives level as percentage of lagoonHeight
	 darkness(60),
	 camera(indoor),
	 frames(120),
	 rotate(false),
	 defaultIP('192.168.2.139'),  % belongs in snapshot
         userpwd('&user=scrapsec&pwd=lakewould'),
         brightness(200),
         brightnessCmd('/camera_control.cgi?param=1&value='),
         contrast(4), 
         contrastCmd('/camera_control.cgi?param=2&value='),
         picCmd('/snapshot.cgi?resolution=32&user=scrapsec&pwd=lakewould'),
         screen(60, 74, point(200,4)),
	 layout([
		 cellstat(cellstat,below,[od(0.4),temp(37.0),shape(20,7),CF]),
		 spacer(     x1, next_row, [color(blue)]),
		 snapshot(  cam, next_row, [ image('mypic1.jpg'),shape(30,30)]),
		 spacer(     x2, next_row, []),
		 lagoon( lagoon1, next_row, [temp(35.0), LS, SF]),
		 lagoon( lagoon2, right,    [temp(35.0), LS, SF]),
		 lagoon( lagoon3, right,    [temp(34.5) ,LS, SF]),
		 spacer(      x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [shape(40,6),SF])
                ])
	 ]) :-
 LS = shape(24,5),
% LF = font(font(times,roman,14)),
 CF = font(font(times,roman,18)),
 SF = font(font(times,roman,20)).





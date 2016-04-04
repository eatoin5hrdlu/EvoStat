config( [
	 updateCycle(20),
	 debugpause(100),
	 numLagoons(2),
	 cellstatRegion(30,100,200,350),
	 cellstatScale(1500),
	 cellstatOffset(100),
%         cellstatContrast(2,1.4,-80),
         cellstatContrast(3,2,-80),
         lagoonRegion(264,45,468,637),
	 lagoonHeight(120),    % divisor for levelScale
	 lagoonWidth(60),
%         lagoonContrast(2,1.35,-70),
         lagoonContrast(2,1.76,-80),
	 levelScale(140),
	 levelOffset(10),
	 levelScale(100),   % 100 gives level as percentage of lagoonHeight
	 darkness(60),
	 camera(indoor),
	 mac(0),
	 frames(120),
	 rotate(90),
	 defaultIP('192.168.2.139'),  % belongs in snapshot
         userpwd('&user=scrapsec&pwd=lakewould'),
         brightness(200),
         brightnessCmd('/camera_control.cgi?param=1&value='),
         contrast(4), 
         contrastCmd('/camera_control.cgi?param=2&value='),
         picCmd('/snapshot.cgi?resolution=32&user=scrapsec&pwd=lakewould'),
         screen(50, 74, point(200,4)),
         imageSize(580,440),
	 layout([
		 cellstat(cellstat,below,[od(0.4),temp(37.0),shape(20,9),CF]),
		 spacer(     x1, next_row, [color(blue)]),
		 snapshot(  cam, next_row, [ image('mypic1.jpg'),shape(30,30)]),
		 spacer(     x2, next_row, []),
		 lagoon( lagoon1, next_row, [temp(35.0), LS, SF]),
		 lagoon( lagoon2, right,    [temp(35.0), LS, SF]),
		 lagoon( lagoon3, right,    [temp(34.5) ,LS, SF]),
		 spacer(      x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [shape(40,8),SF])
                ])
	 ]) :-
 LS = shape(24,8),
% LF = font(font(times,roman,14)),
 CF = font(font(times,roman,18)),
 SF = font(font(times,roman,20)).



% Simulated devices in the Arduino Mega2560 box
%
% bt_device(labcellstat,   '98:D3:31:90:29:0E').
% bt_device(cellstat,     '98:D3:31:90:29:0E').
% bt_device(autosampler,  '98:D3:31:40:1D:D4').
%           '98:D3:31:20:23:4F',
%           '98:D3:31:70:2B:70',
%
% Splatspace simulator
%bt_device( cellstat,     '98:D3:31:90:2B:82').
%bt_device(  lagoon1,     '98:D3:31:70:2A:22').
%bt_device(  lagoon2,     '98:D3:31:40:31:BA').
%bt_device(  lagoon3,     '98:D3:31:20:2B:EB').
%
% Museum simulator
    
%bt_device( cellstatd,     '98:D3:31:40:90:13').
%bt_device(  lagoond1,     '98:D3:32:30:42:6A').
%bt_device(  lagoond2,     '98:D3:31:30:95:60').
%bt_device(  lagoond3,     '98:D3:31:80:34:39').
%bt_device(autosampler,    '98:D3:31:30:95:4B').


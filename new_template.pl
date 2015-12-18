% template.pl  -- Copy this file to <hostname>.pl and edit to make changes

% Component Bluetooth Addresses
% component names which appear in config/1 below.
    
device_address(cellstatA, '98:d3:31:20:23:4f', bluetooth).
device_address(cellstatB, '98:d3:31:90:2b:82', bluetooth).
device_address(lagoon,    '98:d3:31:70:2b:70', bluetooth).
device_address(lagoon1,   '98:d3:31:70:3b:34', bluetooth).
device_address(lagoon2,   '98:d3:31:40:31:ba', bluetooth).

% URL commands for IP Cameras
device_address(      indoor, 'c4:d6:55:34:8d:07', '172.16.3.136').
device_userpwd(      indoor, '&user=scrapsec&pwd=lakewould'),  % Login for IP cameras
device_brightnessCmd(indoor, '/camera_control.cgi?param=1&value=11').
device_contrastCmd(  indoor, '/camera_control.cgi?param=2&value=40').
device_picCmd(       indoor, '/snapshot.cgi?resolution=32&user=admin&pwd=lakewould').

device_address(      outdoor, '00:62:6e:4f:17:d9', '172.16.3.123').
device_userpwd(      outdoor, '&usr=scrapsec&pwd=lakewould').
device_brightnessCmd(outdoor,':88/cgi-bin/CGIProxy.fcgi?cmd=setBrightness&brightness=11').
device_contrastCmd(  outdoor, ':88/cgi-bin/CGIProxy.fcgi?cmd=setContrast&constrast=40').
device_picCmd(       outdoor, ':88/cgi-bin/CGIProxy.fcgi?cmd=snapPicture2&usr=scrapsec&pwd=lakewould').

% commands for USB Cameras
device_address(lifecam,   0, usb).
device_brightnessCmd(lifecam, [ucvdynctrl, '-s', 'Brightness', 11]).
device_contrastCmd(  lifecam, [ucvdynctrl, '-s', 'Contrast', 40]).

device_address(logitek,   0, usb).
device_brightnessCmd(logitek, [ucvdynctrl, '-s', 'Brightness', 11]).
device_contrastCmd(  logitek, [ucvdynctrl, '-s', 'Contrast', 40]).

config( [
	 screen(680, 840, point(750,0)),  % Size and placement of GUI
         imageSize(600,500),              % Evostat Internal Image
	 rotate(false),                   % Is camera rotated?
	 % Nominal image/light level settings
	 darkness(60),                    % Average pixel threshold to identify darkness
	 frames(100),                     % Number of frames to integrate for lumosity
         cellstatRegion(30,100,200,350),  % Cellstat boundary in image
         lagoonRegion(640,7,892,700),     % Lagoon array boundary
	 numLagoons(4),
	 lagoonHeight(280),               % divisor for levelScale
	 lagoonWidth(100),                % approx width of visible contents
	 levelScale(100),                 % 100 gives level as percentage of lagoonHeight
	 layout([            % GUI
		 cellstat(cellstatA,below,[od(0.4),temp(37.0),shape(240,60),CF]),
		 spacer(      x1, next_row, [color(blue)]),
		 snapshot(lifecam, next_row, [ shape(600,500),image('mypic1.jpg')]),
		 spacer(      x2, next_row, []),
		 lagoon( lagoon1, next_row, [temp(35.0), LS, LF]),
		 lagoon( lagoon2, right,    [temp(35.0), LS, LF]),
		 lagoon( lagoon3, right,    [temp(35.0), LS, LF]),
		 lagoon( lagoon4, right,    [temp(35.0), LS, LF]),
		 spacer(      x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [ shape(400,30),SF])
                ])
	 ]) :-
 LS = shape(142,60),
 LF = font(font(times,roman,14)),
 CF = font(font(times,roman,18)),
 SF = font(font(times,roman,20)).

config( [
	 textMessages(14400),
	 updateCycle(40),
	 debugpause(10),
         cellstatRegion(80,120,300,350),
         cellstatContrast(2,1.4,-90),
	 cellstatHeight(220),   % 100% full line

         lagoonRegion(330,20,510,470),
         lagoonContrast(2,1.4,-90),
	 lagoonHeight(180),     % 100% full line

	 lagoonWidth(100),

	 numLagoons(4),

	 cellstatScale(1500),
	 cellstatOffset(100),

	 screen(60,90,point(200,1)),
         imageSize(580,440),

	 levelScale(140),   % Maximum percentage or mL
	 levelOffset(10),   % Minimum percentage or mL
	 frames(100),       % lumosity integration
	 darkness(60),      % Average pixel thresh dark
	 camera(0),
	 rotate(90),
	 layout([
		 cellstat(cellstatb,below,[od(0.4),temp(37.0),shape(20,8),CF]),
		 spacer(        x1, next_row, [color(blue)]),
		 snapshot(     cam, next_row, [ image('mypic1.jpg'),shape(30,30)]),
		 spacer(        x2, next_row, []),
		 lagoon( lagoon1, next_row, [temp(35.0), LS, SF]),
		 lagoon( lagoon2, right,    [temp(35.0), LS, SF]),
		 lagoon( lagoon3, right,    [temp(34.5) ,LS, SF]),
		 lagoon( lagoon4, right,    [temp(34.5) ,LS, SF]),
		 spacer(        x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [shape(40,6),SF])
                ])
	 ]) :-
 LS = shape(24,7),
% LF = font(font(times,roman,14)),
 CF = font(font(times,roman,18)),
 SF = font(font(times,roman,20)).
<
% BETABOX simulator
bt_device(cellstatb,     '98:D3:31:90:2B:82').
bt_device(  lagoon1,     '98:D3:31:70:2A:22').
bt_device(  lagoon2,     '98:D3:31:40:31:BA').
bt_device(  autosampler, '98:D3:31:20:2B:EB').

simulator.    % Avoid excessive component handshaking
deadzone(1).  % Min change for PID to modify valve times

% logfile(logfile).
    
% Simulated devices in the Arduino Mega2560 box
%
% bt_device(labcellstat,   '98:D3:31:90:29:0E').
% bt_device(cellstat,     '98:D3:31:90:29:0E').
% bt_device(autosampler,  '98:D3:31:40:1D:D4').
%           '98:D3:31:20:23:4F',
%           '98:D3:31:70:2B:70',
%
% BETA
%bt_device( cellstat,     '98:D3:31:90:2B:82').
%bt_device(  lagoon1,     '98:D3:31:70:2A:22').
%bt_device(  lagoon2,     '98:D3:31:40:31:BA').
%bt_device(  lagoon3,     '98:D3:31:20:2B:EB').
%
% Kirkland
%bt_device( cellstatd,     '98:D3:31:40:90:13').
%bt_device(  lagoond1,     '98:D3:32:30:42:6A').
%bt_device(  lagoond2,     '98:D3:31:30:95:60').
%bt_device(  lagoond3,     '98:D3:31:80:34:39').
%bt_device(autosampler,    '98:D3:31:30:95:4B').

% pid(Id, Kp, Ki, Kd, Target, Min, Max, DeltaT)

pid_controllers([
   pid(cellstat,0.4, 0.3, 0.3, 85, 10, 100, 60),
   pid(lagoon1, 0.4, 0.3, 0.3, 30, 10, 100, 30),
   pid(lagoon2, 0.4, 0.3, 0.3, 30, 10, 100, 30),
   pid(lagoon3, 0.4, 0.3, 0.3, 30, 10, 100, 30),
   pid(lagoon4, 0.4, 0.3, 0.3, 30, 10, 100, 30)]).

control(cellstat1,level, 'v0', autosampler, 'm').
control(lagoon3,  level, 'v1', autosampler, 'k').
control(lagoon2,  level, 'v1', autosampler, 'j').
control(lagoon1,  level, 'v1', autosampler, 'i').

%%%%%%%%%%%%%% SYSTEM/USER DEPENDENT STUFF 

% To build stand-alone executable there are different emulators
:- discontiguous evostat_directory/1, python/1, os_emulator/1.

% DEFAULTS FOR WINDOWS
evostat_directory('C:\\cygwin\\home\\peterr\\src\\EvoStat\\') :- windows.
python('C:\\Python27\\python.exe')                            :- windows.
os_emulator('C:\\cygwin\\swipl\\bin\\swipl-win.exe')          :- windows.

% OPTIONS FOR WINDOWS
% evostat_directory('C:\\cygwin64\\home\\Owner\\src\\EvoStat\\') :- windows.
% python('C:\\cygwin\\Python27\\python.exe').
% python('C:\\cygwin64\\Python27\\python.exe').
% os_emulator('C:\\cygwin\\pl\\bin\\swipl-win.exe').


% DEFAULTS FOR LINUX
evostat_directory('/home/peter/src/EvoStat/') :- linux.
python('/usr/bin/python')                     :- linux.
os_emulator('/swipl/bin/swipl')        :- linux, pce_autoload_all, pce_autoload_all.

% OPTIONS FOR LINUX
% os_emulator('/usr/bin/swipl')        :- linux, pce_autoload_all, pce_autoload_all.
% os_emulator('/home/peter/bin/swipl') :- linux, pce_autoload_all, pce_autoload_all.
% os_emulator(swi('bin/xpce-stub.exe')):- linux, pce_autoload_all, pce_autoload_all.
% os_emulator(swi('bin/swipl-win.exe')):- linux, pce_autoload_all, pce_autoload_all.
    

% RUNTIME LOADING OF SHARED OBJECT

load_bluetooth :- 
  ( windows
    -> load_foreign_library(foreign(plblue))
    ; load_foreign_library(plblue)
  ).
% Windows??
% load_foreign_library('C:\\cygwin\\home\\peter\\src\\EvoStat\\plblue'),

% COMPILE TIE LOADING OF SHARED OBJECT
% :- ( current_prolog_flag(arch,'i386-win32')
%     -> load_foreign_library(foreign(plblue))
%     ;  load_foreign_library(plblue)
%   ),
%   writeln('plblue (BLUETOOTH) loaded').

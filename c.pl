%:- use_foreign_library(dlltest). 
%:- rlc_color(window, 150,150,200).
:- use_module(library(time)).
:- use_module(library(pce)).

:- free(@'_dialog_bg'),  % This should make the background light-blue steel
   XR is 0xB0*257, XG is 0xC4*257, XB is 0xDE*257,
   new(@'_dialog_bg', colour(@default,XR,XG,XB)).

:- use_module(library(process)).
:- use_module(library(charsio)).
:- use_module(library(helpidx)).
:- use_module(library(lists)).
:- use_module(library(ctypes)).

:- dynamic debug/0. % Removed by save_evostat when building binary
debug.

timeline(S):-
    get_time(Now),
    convert_time(Now,String),
    write(S,String),nl(S).

:- dynamic component/3, leak/1, temperature/3, last_level/2.

% Configuration <hostname>.pl settings
:- dynamic bt_device/2, logfile/1, simulator/0, deadzone/1, watcher/3.
:- multifile bt_device/2, logfile/1, simulator/0, deadzone/1, watcher/3.
% logfile(logfile).

temp_file('mypic1.jpg').
temp_file('mypic2.jpg').
temp_file('dbg.txt').
temp_file(File)       :- logfile(File).

:- dynamic timer_time/1.
timer_left(T) :-
    get_time(Now),
    timer_time(Last),
    T is integer(Now-Last).
    
cleanup :- temp_file(F), exists_file(F), delete_file(F) ; true.

% All messages to logfile (otherwise, message window)

logging :-
    ( logfile(File)
     -> tell(File),
	telling(S),
	set_stream(S,buffer(line)),
	set_stream(user_error,buffer(line)),
	set_stream(S,alias(user_error))
     ;  true
    ).

camera_device(Device) :-
    param(camera(Num)),
    integer(Num),
    !,
    concat_atom(['/dev/video',Num],Device).

camera_device(_) :- writeln('No Camera Device'),fail.

camera_reset :- windows, !.   % No camera reset on Windows

camera_reset :-               % No camera to reset
    camera_device(Device),
    \+ access_file(Device,exist),
    writeln(no_camera),
    !.

camera_reset :-
    evostat_directory(Dir),
    Cmd = '/usr/bin/uvcdynctrl',
    config_name(Config,_),    % Hostname or evostat argument
    concat_atom([Config,'.gpfl'], Settings),
    camera_device(Device),
    concat_atom(['--device=',Device],Option),
    Args = ['-L', Settings, Option],
    writeln(resettingCamera(Dir, Cmd, Args)),
    process_create(Cmd,Args,[cwd(Dir)]),
    writeln(resetCamera).

windows :- current_prolog_flag(windows,true).
unix    :- current_prolog_flag(unix,true).

evostat_directory('C:\\cygwin\\home\\peterr\\src\\EvoStat\\') :-
  gethostname('ncmls8066.ncmls.org'),!.
evostat_directory('C:\\cygwin\\home\\peter\\src\\EvoStat\\')  :- windows, !.
evostat_directory('/home/peter/src/EvoStat/').
	
python('C:\\Python27\\python.exe')         :- gethostname(elapse),!.
python('C:\\cygwin\\Python27\\python.exe') :- windows, !.
python('/usr/bin/python').

:- [gbutton]. % Micro-controllers as PCE objects
:- [dialin].  % Pop up Aduino dialog interface
:- [adjust].  % PID controller <-> PCE interface

:- dynamic screen/5, input/2.

:- dynamic component/2,
           levelStream/2,
	   air/0,
	   mix/0,
	   toggle_auto/0.

%
% Copy and edit template.pl to create a configuration <hostname>.pl
% (or <evostatname>.pl if more than one on the same computer).
% 'evostat' reads this file and creates <hostname>.settings
% for Python programs (ipcam.py, level.py, fluor.py, pH.py)
%
% You can edit the .settings file to test/debug the Python
% programs, but it will be overwritten when 'evostat' runs.
%
% Color code for labeling text (name or parameter values):
%   Red before connections are established
%   Blue when parameter values are low
%   Orange when parameters are high
%   Green when paramaters are in the target range
%
:- use_module(library(lists), [ flatten/2 ] ).

:- dynamic config/1.       % Loaded configuration
:- dynamic file_modtime/2. % Modification time of loaded file
:- dynamic supress/1.      % Exclude terms from Python dictionary

param(P) :- config(List), memberchk(P,List).

check_file(Root,File) :-  % consult(foo) works files 'foo' or 'foo.pl'
	( exists_file(Root)
        -> true
        ; concat_atom([Root,'.pl'],File),
	  exists_file(File)
        ).

% config_name(-Root,-File) is on command line or <hostname>
config_name(Root,File) :-
	current_prolog_flag(argv,[_Exe|Args]),  % Command-line argument
	member(Root,Args),
	check_file(Root,File),
	!.

config_name(Root,File) :-
	gethostname(Name),    % <HOSTNAME>.pl configuration file
	atom_chars(Name,Cs),
	( append( RCs,['.'|_],Cs ) % Eliminate domain name ('x.y.com')
        -> atom_chars(Root,RCs)
        ;  Root = Name
        ),
	check_file(Root,File).

% Grammar to convert Prolog term to Python dictionary
:- dynamic tabs/1.
tabs(1).

writePythonParams(EvoStat) :-    
    config(List),
    pl2PythonDict(List, PyString),        % Convert to Python Dictionary
    concat_atom([EvoStat,'.settings'],File), % Write it out
    tell(File),
    write(PyString),
    told,
    current_prolog_flag(argv,Args),
    ( memberchk(gen,Args) -> halt ; true ).

pl2PythonDict(Data, PyAtom) :-       % Generate a Python dictionary string
	pl2py(Data, PythonDict, []),
	flatten(['{\n',PythonDict,'}\n'], Flat),
	concat_atom(Flat, PyAtom).

quote_atom(false,'None') :- !.
quote_atom([A],Q) :- quote_atom(A,Q).
quote_atom(N,N)   :- number(N).
quote_atom(A,Q)   :- atom(A), concat_atom(['''',A,''''],Q).

% DCG versions
quote_atom(false) --> ['None'].  % None is Python's 'false'
quote_atom([A])   --> quote_atom(A).
quote_atom(N)     --> {number(N)},[N].
quote_atom(A)     --> {atom(A)}, ['''',A,''''].

quote_atoms([],[], _).
quote_atoms([A],[Q],_) :- quote_atom(A,Q).
quote_atoms([A|T],[Q,Spacer|QT],Spacer) :- quote_atom(A,Q), quote_atoms(T,QT,Spacer).

%DCG versions
quote_atoms([],_)         --> [].
quote_atoms([A],_)        --> quote_atom(A).
quote_atoms([A|T],Spacer) --> quote_atom(A), [Spacer], quote_atoms(T,Spacer).

tabin  --> { retract(tabs(N)), NN is N + 1, assert(tabs(NN)) }.
tabout --> { retract(tabs(N)), NN is N - 1, assert(tabs(NN)) }.

indent     --> { tabs(N) }, indent(N).
indent(N)  --> { N<1},  !.
indent(N)  --> { N>0, NN is N-1}, indentation, indent(NN).

indentation --> ['       '].

pl2py([])    --> !, [].
pl2py(Term)  --> { supress(Term)  }, !, [].
pl2py(A)     --> { quote_atom(A,Q)}, !, [Q].
pl2py([H])   --> !, pl2py(H), [' \n'].
pl2py([H|T]) --> !, indent, pl2py(H), pl2py(T).

pl2py(Term) --> { Term =.. [F|[A]],   % SINGLE f(a)
	          quote_atom(F,QF),
	          quote_atom(A,QA),
		  !
                },
		[QF, ' :  ', QA, ',\n'].

pl2py(Term) --> { Term =.. [F|[A|As]],    % TUPLE   f(a,b,...)
	          quote_atom(F,QF),
	          quote_atoms([A|As], Str, ','),
                  !
                },
	        [QF,' : (', Str, '),\n' ].

pl2py(Term) --> { Term =.. [F|Args],  % NESTED DICTIONARY  f(g(a),...)  
	          quote_atom(F, QF)
	        },
		[QF, ' :  {'],
		pl2py(Args),
		['      }'].
                 
% Additional messages: pathe_report/1 called on exit.
% Call append/1 because output logfile has been closed.

pathe_report(verbose) :-
    logfile(File),
    append(File),
    writeln(verbose_test_report1).

pathe_report(moderate) :-
    logfile(File),
    append(File),
    writeln(moderate_test_report).

pathe_report(_) :-
    writeln('No Logging Enabled').

freeall :-
    get(@gui, graphicals, Chain),
    chain_list(Chain, CList),
    maplist(free,CList).

%
% Find the Nth lagoon(object), names can be complex
% but must end with a single digit (usually 1-4)
% E.g. lagoonDarwin2, lagoon_huxley_1, cuvierVirustat4
%
lagoon_number(Object, N) :-
    component(Lagoon, lagoon, Object),
    atom_codes(Lagoon, Codes),
    Digit is N + 0'0,
    append(_,[Digit],Codes).

% Alternative to for_all ?grapicals
lagoons(Cmd) :-
    component(_, lagoon, Object),
    send(Object,Cmd),
    fail.
lagoons(_).

% Many responses from the micro-controllers contain
% data to be stored in corresponding PCE (GUI) objects.

send_info(end_of_file,_) :-
   write('Is debugging on? Possibly calling Cellstat '),
   writeln('level detection while working on Lagoons'),
   fail.
    
send_info(flux(F),Stream) :- !, newFlux(flux(F),Stream).
send_info(level(Level),_) :-  % Single value is Cellstat level/1
    component(_, cellstat, Cellstat),
    send(Cellstat, setLevel, Level).

send_info(Term,_) :-  % levels/N: arity equal to the number of lagoons
    functor(Term,levels,_),
    arg(N, Term, Level),
    lagoon_number(Object, N),   % position correspnds to lagoon
    send(Object, setLevel, Level),
    fail.

send_info(Msg,_) :- writeln(send_info(Msg)).

% Code to launch python/OpenCV level detection programs

check_error(camera(IP))    :- writeln(error(camera(IP))),!,fail.
check_error(othererror(D)) :- writeln(error(othererror(D))),!,fail.
check_error(_).               % Everything else is not an error

get_level(Type) :-
    python(Python),
    evostat_directory(Dir),
    concat_atom([Dir,'ipcam.py'],IPCAM),
    CmdLine = [IPCAM,Type],
    ( retract(levelStream(Type,Previous)) ->
	catch( read(Previous, Info),
	       Ex,
	       (writeln(caught(Ex,CmdLine)),sleep(3),fail)),
        check_error(Info),
	send_info(Info, Previous),
	close(Previous)
     ; true
    ),
    evostat_directory(Dir),
    writeln(launching(Python,CmdLine)),
    process_create(Python,CmdLine,
		   [stdout(pipe(Out)), stderr(std), cwd(Dir)]),
    assert(levelStream(Type,Out)),
    writeln(launched),
    !.

get_level(Type) :- 
    writeln(failed(levelUpdate(Type))).

newFlux(end_of_file,_) :- !.
newFlux(FluxTerm, Stream) :-
	FluxTerm =.. [Lagoon,FluxValue],
	send(@Lagoon, setFlux, FluxValue),
	catch(read(Stream, NextTerm),Ex,(writeln(caught(Ex)),fail)),
	newFlux(NextTerm, Stream).

:- pce_begin_class(evostat, dialog, "PATHE Control Panel").

initialise(W, Label:[name]) :->
          "Initialise the window and fill it"::
          send_super(W, initialise(Label)),
	  screen(WW,WH,DW,DH,Location),
	  EWidth is WW*DW/100,
	  EHeight is WH*DH/100,
          send(W, size, size(EWidth, EHeight)),
	  writeln(evostat(width(EWidth),height(EHeight))),

% MENU BAR
	  send(W,  append, new(MB, menu_bar)),
	  send(MB, label_font(huge)),

	  % Long update cycle for communication with subsystems
	  new(Msg1, message(W, autoUpdate)),  % Create Timer Object
	  free(@ut),
	  param(updateCycle(Seconds)),
	  send(W, attribute, attribute(timer, new(@ut, timer(Seconds, Msg1)))),

	  % Short update cycle for GUI
	  new(Msg2, message(W, fastUpdate)),  % Message for fast Timer
	  free(@ft),
	  param(updateCycle(Seconds)),
          Frequent is integer(Seconds/10),
	  writeln(frequentTimer(Frequent)),
	  send(W, attribute, attribute(timer, new(@ft, timer(Frequent, Msg2)))),
	
	  % Status updates via Text Messaging
	  new(Msg3, message(W, sendTexts)),
	  free(@tt),
	  param(textMessages(TextSeconds)),
	  send(W,attribute,attribute(timer,new(@tt,timer(TextSeconds,Msg3)))),

	  send(MB, append, new(File, popup(file))),
          free(@action),
	  send_list(File, append, [ menu_item(ok,message(W, return, ok)),
				    menu_item(quit,message(W, quit)) ]),
	  send(MB, append, new(@action, popup(action))),
	  send_list(@action, append,[ menu_item('Drain Lagoons',
					message(W, drain, lagoons)),
				    menu_item('Drain Cellstat',
				        message(W, drain, cellstat)),
				    menu_item(stop,
					      message(W, stopped)),
				    menu_item(start,
					      message(W, started)),
				    menu_item(pIDoff,
					      message(W, stopPID)),
				    menu_item(pIDon,
					      message(W, startPID)),
				    menu_item(texting,
					      message(W, sendText)),
				    menu_item('No Texting',
					      message(W, stopText))
				  ]),
	  send(MB, append, new(Help, popup(help))),
	  about_atom(About),
	  send_list(Help, append, [ menu_item(about,
					message(@display, inform, About)),
				    menu_item(debug,
					message(@prolog, manpce))]),
         call(Label,Components),
         findall(_,(component(_,_,Obj),free(Obj)),_), % Clear out previous
	 maplist(create(@gui), Components),

	 initPID,                     % Start PID controllers
         send(@action?members, for_all,
	      if(@arg1?value==pIDon,message(@arg1, active, @off))),
         send(@action?members, for_all,
	      if(@arg1?value==pIDoff,message(@arg1, active, @on))),

         send(W,started),
         send_super(W, open, Location).

drain(_W, What) :->  writeln(draining(What)).

stopped(_W) :->
       send(@ft,stop),  % Stop the GUI update timer as well
       writeln('Stopping Level Detection (image processing)'),
       send(@ut, stop),
       send(@action?members, for_all,
	    if(@arg1?value==stop,message(@arg1, active, @off))),
       send(@action?members, for_all,
	    if(@arg1?value==start,message(@arg1, active, @on))),
       writeln(stopped).

started(_W) :->
       writeln(starting),
       send(@ut, start),
       send(@action?members, for_all,
	    if(@arg1?value==start,message(@arg1, active, @off))),
       send(@action?members, for_all,
	    if(@arg1?value==stop, message(@arg1, active, @on))),
       get_time(Now),
       INow is integer(Now),
       retractall(timer_time(_)),
       assert(timer_time(INow)),
       send(@ft,start),       % Now restart the fast GUI update timer
       writeln('Starting Level Detection (Image processing)').

stopPID(_W) :-> 
    pidstop,
    send(@action?members, for_all,
	 if(@arg1?value==pIDoff,message(@arg1, active, @off))),
    send(@action?members, for_all,
	 if(@arg1?value==pIDon,message(@arg1, active, @on))).

startPID(_W) :->
    pidstart,
    send(@action?members, for_all,
	 if(@arg1?value==pIDon,message(@arg1, active, @off))),
    send(@action?members, for_all,
	 if(@arg1?value==pIDoff,message(@arg1, active, @on))).

cellstat(Self) :-> "User pressed the CellStat button"::
                   send(Self,stopped),
		   ( simulator -> writeln(simulator)
                   ;              send(Self,manualUpdate)
                   ).

lagoon1(_W) :->  "User selected Lagoon 1"::
                 component(lagoon1,lagoon,L),
		 ( toggle_auto
	         ->  retract(toggle_auto), Cmd = 'a0'
		 ;   assert(toggle_auto), Cmd = 'a1'
 	         ),
		 send(L,converse,Cmd).

lagoon2(_W) :->   "User selected Lagoon 2"::
                  component(lagoon2,lagoon,L),
		  send(L,calibrate).

lagoon3(_W) :->  "User selected Lagoon 3"::
                 component(lagoon3,lagoon,L),
		 ( toggle_auto
		 ->  retract(toggle_auto), Cmd = 'o2'
		 ;   assert(toggle_auto), Cmd = 'o-'
	         ),
		 send(L,converse,Cmd).

lagoon3d(_W) :-> "User selected Lagoon 3-Darwin"::
                 component(lagoon3d,lagoon,L),
		 ( toggle_auto
		 -> retract(toggle_auto), Cmd = 'o2'
		 ;  assert(toggle_auto), Cmd = 'o-'
	         ),
		 send(L,converse,Cmd).

lagoon4(_W) :->	"User selected Lagoon 4"::
                component(lagoon4,lagoon,L),
		send(L,calibrate).

quit(W) :->
       "User pressed the Quit button"::
        send(W, return(quit)).

load(W, File:[file]) :->
        "User pressed the Load button"::
%	send(@ut, stop),     % Shut down the label update timerg
        send(W, return(File)).

ok(W) :->
        "User pressed the Ok button"::
        send(W, return(ok)).

prompt(W, Value:name) :<-
        "Open it, destroy it and return the result"::
        get(W, confirm, Value).

% Green when within 5% of Target magnitude
range_color(Target, Current, Color) :-
    Delta is Target/20,
    Max is Target + Delta,
    Min is Target - Delta,
    (  Current > Max -> Color = red
     ; Current < Min -> Color = blue
     ;                  Color = darkgreen
    ).

% Updating can take a long time, so we must:
% 1) Stop auto-update
% 2) Reload the  <hostname>.pl user parameter file if it changed
% 3) Perform the update on all components
% 4) Restart the auto update timer (new timer value from settings NYI)
% 5) Make a fast_update version for the GUI

autoUpdate(Self) :->
    send(@gui, stopped),
    update_config(_),          % Re-load if change
    send(Self,quiet),      writeln(sent(quiet)),
    send(Self,readLevels), writeln(sent(readlevels)),
    send(Self,mixon),      writeln(sent(mixon)),
    send(@gui, started),
    report.

quiet(Self) :->
    simulator -> true ;
    send(Self?graphicals, for_all,  % Lagoon mixers OFF
	 if(message(@arg1,instance_of,lagoon),message(@arg1,converse,'m0'))),
    component(_,cellstat,Cellstat),
    send(Cellstat,converse,'m0'),
    send(Cellstat,converse,'o-').

mixon(Self) :->
    writeln('Updating ebuttons'),
    send(Self?graphicals, for_all,
	 if(message(@arg1,instance_of,ebutton),message(@arg1,update))),
    writeln('Updating snapshot'),
    send(Self?graphicals, for_all,
	 if(message(@arg1,instance_of,snapshot),message(@arg1,update))),
    writeln('Turning noisy stuff back on'),
    send(Self?graphicals, for_all,  % Lagoon mixers OFF
	 if(message(@arg1,instance_of,lagoon),message(@arg1,converse,'m1'))),
    component(_,cellstat,CellStat),
    send(CellStat,converse,'m1'),
    send(CellStat,converse,'o2'),
    writeln('Updated:Air and Mixers On').
    
readLevels(_) :->
    writeln(read_lagoon_levels),
    get_level(lagoons), writeln(after(get_level(lagoons))),
    sleep(10),
    writeln(read_cellstat_level),
    get_level(cellstat), writeln(after(get_level(cellstat))),
    writeln(read_levels(finished)).

% Put things to be refreshed more often here:
% Image update, time to next level detection, etc.
% Currently only the autosampler/next cycle time indication

fastUpdate(Self) :->
    send(Self?graphicals, for_all,
	 if(message(@arg1,instance_of,sampler),message(@arg1,fast_update))).

sendText(Self) :->
    send(Self,sendTexts),
    send(@tt,start),
    send(@action?members, for_all,
	 if(@arg1?value=='No Texting',message(@arg1, active, @on))),
    send(@action?members, for_all,
	 if(@arg1?value==texting,message(@arg1, active, @off))).

stopText(_S)   :->
    send(@tt,stop),
    send(@action?members, for_all,
	 if(@arg1?value==texting,message(@arg1, active, @on))),
    send(@action?members, for_all,
	 if(@arg1?value=='No Texting',message(@arg1, active, @off))).
    
    
sendTexts(_Self) :->
    writeln(sending_texts),
    findall(W,(watcher(W),concat_atom(['./smstext.py ',W],C),shell(C)),Ws),
    writeln(sent_to(Ws)).
    
:- pce_end_class.  % End of evostat

% Initializers are extra arguments to the constructor
% Data is a list of messages to continue initializing the object

create(Dialog, Component) :-
	Component =.. [Type, Name, Position, Data],
	free(@Name),
	Class =.. [Type,Name],
	new(@Name, Class),
	maplist(send(@Name), Data), % Process all before appending
	send(Dialog, append(@Name, Position)),
        assert(component(Name,Type,@Name)).

add_reset(Dialog,@Name) :-
    ( send(@Name,instance_of,ebutton) ->
	  writeln(itsanebutton(@Name)),
	  new(Anon,reset('R',@Name)),
	  send(Dialog,append(Anon,right))
     ; writeln(notsomuch(@Name))
     ).

about_atom(About) :-
        open('evostat.about', read, Handle),
	read_pending_input(Handle,FileContent,[]),
	atom_chars(About,FileContent).

% gethostname returns the full domain name on some systems
hostname_root(H) :-
     gethostname(Name),
     atom_chars(Name,Cs),
     ( append(RCs,['.'|_],Cs) -> atom_chars(H,RCs) ; H = Name ).

% Bluetooth interface, with error checking and fail messages
% bluetalk(+Socket, +Cmd, -Reply).

bluetalk(@nil,  _,  'no_connection.'  ).
bluetalk(   _,  '', 'nothing_to_send.').
bluetalk( S, Cmd, Reply) :- bt_converse(S ,Cmd, Reply).
bluetalk(   _,   _, 'send_failed.'    ).


% Create prolog executable (saved-state) with goal [c],save_evostat.
% Configuration <name> is either a command-line argument or <hostname>
% The corresponding Prolog file ( <name>.pl ) must exist.
% Load configuration and generate Python .settings (dictionary) file.
% Many dynamic predicates (config, debug options ) from <hostname>.pl

update_config(Config) :-      % Load or reload when file changes
    config_name(Config,File),  %  Find configuration name (hostname or cmd arg)
    load_newest(Config,File).

load_newest(_,File) :-
    file_modtime(File, Time),                  % Mod time when File was loaded
    source_file_property(File,modified(Time)), % Matches!
    !.

load_newest(Config,File) :-
    writeln(consulting(File)),
    consult(Config),
    writeln(consulted(Config)),
    source_file_property(File,modified(Time)),
    retractall(file_modtime(File,_)),
    assert(file_modtime(File, Time)).

c :- main([]).

c(Name) :-
    free(@gui),
    new(@gui, evostat(Name)),
    send(@gui?frame, icon, bitmap('./evo.xpm')),
    get(@gui, prompt, Reply),
    (Reply = quit ->
         send(@ft, stop),
         send(@ut, stop),
         send(@gui, destroy)
     ;   writeln(Reply)
    ).

report :-
    open('evostat.report', append, S),
    nl(S), timeline(S),
    ( leak(Type)             -> write(S,'leak '),write(S,Type),nl(S) ; true ),
    ( temperature(cellstat,_,Val) ->
        write(S,'Host at '),write(S,Val),write(S,'C'),nl(S),
        write(S,'OD600 '),write(S,'N/A'),nl(S)
    ; true
    ),
    ( temperature(lagoon,_,LVal) ->
        write(S,'Lagoon Tm '),write(S,LVal),write(S,'C'),nl(S)
    ; true
    ),
    close(S).

main :-  open('evostat.report', append, S),
	 nl(S),nl(S),write(S,'EvoStat started:'),timeline(S),
	 close(S),
	 pce_main_loop(main).

main(_Argv) :-
        assert(file_search_path('C:\\cygwin\\home\\peter\\src\\EvoStat')),
%        use_module(library(pce)),
    
        ( logfile(_), windows
         -> retract(logfile(_))
         ; true
        ),
        evostat_directory(HomeDir),  % With this, the savestate
        cd(HomeDir),               % can be executed from anywhere
        cleanup,                   % uses temp_file/1 to remove files
	logging,                   % Send output to F if logfile(F) defined

	% Delay a bit if the computer is just starting up (low PID)
        current_prolog_flag(pid, PID),
        (PID < 900 -> sleep(30) ; true),

        set_prolog_flag(save_history,false),
	at_halt(pathe_report(verbose)),  % Special exit predicate to call
	( windows
         -> load_foreign_library(foreign(plblue))
	  ; load_foreign_library(plblue)
        ),
%        load_foreign_library('C:\\cygwin\\home\\peter\\src\\EvoStat\\plblue'),
	update_config(Root),
	param(screen(WF,HF,Loc)),    % From configuration data
        get(@display?size,width,Width),
        get(@display?size,height,Height),
        assert(screen(Width,Height,WF,HF,Loc)),
        camera_reset,
	param(layout(Components)),
	Layout =.. [Root,Components],
	assert(Layout),
%	writeln(Layout),

	assert(supress(layout(_))),           % Leave this out of the Python "settings" dictionary
	assert(supress(screen(_,_,_))),       %     ''
        writePythonParams(Root),

	c(Root),
        !.

% To build stand-alone 'evostat', there are different emulators
% Windows  'C:\\cygwin\\swipl\\bin\\swipl-win.exe'
% Cygwin   '/cygdrive/c/cygwin/swipl/bin/swipl-win.exe'
%          '/usr/bin/swipl-win'
% Linux    swi('bin/xpce-stub.exe'), swi('bin/swipl-win.exe')
%

os_emulator('C:\\cygwin\\pl\\bin\\swipl-win.exe') :-
    gethostname(egate), !.   % My windows laptop

os_emulator('C:\\cygwin\\swipl\\bin\\swipl-win.exe') :-
    windows, !.
    
os_emulator('/home/peter/bin/swipl') :- % Haldane at SplatSpace
     gethostname(haldane),              % Custom-build SWI Prolog
     !,
     pce_autoload_all,
     pce_autoload_all.

os_emulator('/usr/bin/swipl') :-  % Linux
     pce_autoload_all,
     pce_autoload_all.

save_evostat :-
    os_emulator(Emulator),
    retractall(debug),
    Options = [stand_alone(true), goal(main)],
    qsave_program(evostat, [emulator(Emulator)|Options]).

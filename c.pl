%:- use_module(library('http/httpd')).        
%:- use_module(library(semweb/rdf_db)).
%:- use_foreign_library(dlltest). 
%:- rlc_color(window, 150,150,200).

:- set_prolog_flag(double_quotes,codes).
:- use_module(library(time)).
:- use_module(library(pce)).

%%%%%%%%%%% INTERNAL WEB SERVER 
:- use_module(library(http/thread_httpd)).   % Server loop
:- use_module(library(http/http_dispatch)).  % dispatch table
:- use_module(library(http/http_header)).    % Terms -> HTML
:- use_module(library(http/html_write)).     % Terms -> HTML
:- use_module(library(http/html_head)).      % html_requires//1

:- dynamic html_syntax/0.      % Assert for HTML dialect(EOL etc.)
html_syntax --> {html_syntax}. % Also callable as a grammar rule

%%%%%%%%%%% OS IDENTIFICATION
windows :- current_prolog_flag(windows,true).
unix    :- current_prolog_flag(unix,true).

%%%%%%%%%%% RUNNING EXTERNAL PROGRAMS (python, etc.)
:- use_module(library(process)).
:- use_module(library(charsio)).
:- use_module(library(helpidx)).
:- use_module(library(lists)).
:- use_module(library(ctypes)).

:- ensure_loaded(webspec).           % HTTP Request handlers
:- ensure_loaded(et).                % Term Expansion for XPCEgen

:- free(@'_dialog_bg'),  % This should make the background light-blue steel
   XR is 0xB0*257, XG is 0xC4*257, XB is 0xDE*257,
   new(@'_dialog_bg', colour(@default,XR,XG,XB)).


message(F,L) :- format(user_error, F, L).

:- dynamic component/3,
           leak/1,
	   temperature/3,
	   last_level/2,
	   cycle/1.

:- dynamic bt_device/2, watcher/2.
:- multifile bt_device/2, watcher/2.

:- dynamic debug/0, logfile/1.
:- multifile debug/0, logfile/1.
% debug.
% logfile(logfile).

:- dynamic err/2.
:- multifile err/2.

:- dynamic webok/0.  % Asserted when evostat class is initialized
:- multifile webok/0.

%%%% LIST ALL TEMPORARY FILES FOR CLEANUP

temp_file('mypic1.jpg').
temp_file('mypic2.jpg').
temp_file('dbg.txt').
temp_file(File)       :- logfile(File).

cleanup :- temp_file(F), exists_file(F), delete_file(F), fail.
cleanup.

%%%%% TIME STUFF

timestring(TString):- get_time(Now), convert_time(Now,TString).
timeline(Stream)   :- timestring(String), write(Stream,String), nl(Stream).

:- dynamic timer_time/1.
timer_left(T) :-
    get_time(Now),
    timer_time(Last),
    T is integer(Now-Last).

timer_reset :-
    get_time(Now),
    INow is integer(Now), % One second resolution
    retractall(timer_time(_)),
    assert(timer_time(INow)).
    
%%%%%% LOGGING
% All messages to logfile (otherwise, message window)
% Never use logfile on Windows
logging :- windows, !, retractall(logfile(_)).
logging :- ( logfile(File)
            -> tell(File),
	       telling(S),
	       set_stream(S,buffer(line)),
	       set_stream(user_error,buffer(line)),
	       set_stream(S,alias(user_error))
	    ; write(user_error,'No Logging at this time'),nl(user_error)
	   ).

%%%%%%%%%%%%%%%% CAMERA
camera_device(Device) :-
    param(camera(Num)),
    integer(Num),
    !,
    concat_atom(['/dev/video',Num],Device).

camera_device(_) :-
    write(user_error,'No Camera Device'),nl(user_error),
    fail.

camera_exists :-
    camera_device(Device),
    access_file(Device,exist).
    
camera_reset :- windows, !.   % Null camera reset on Windows
camera_reset :- \+ camera_exists,  % No camera to reset
		write(user_error,no_camera),nl(user_error),
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
%%%%%%%%%%%%%CAMERA
%% SUBSYSTEMS

:- [gbutton]. % XPCE parent class [ebutton] etc.

:- [adjust].  % PID controller <-> PCE interface

:- dynamic screen/5.
:- dynamic levelStream/2,
	   air/0,
	   mix/0,
	   toggle_auto/0.
%
% Create configuration file <hostname>.pl from  template.pl
% (or <evostatname>.pl if more than one on the same computer).
% 'evostat' reads this file and creates <hostname>.settings
% for Python programs (levels.py, level.py, fluor.py, pH.py)
%
% You can edit the .settings file to test/debug the Python
% programs, but it will be overwritten when 'evostat' runs.
%
% Color code for labeling text (name or parameter values):
%   Red before connections are established
%   Blue when values are low
%   Orange when values are high
%   Green when in the target range
%
:- use_module(library(lists), [ flatten/2 ] ).

:- dynamic config/1.       % Loaded configuration
:- dynamic file_modtime/2. % Modification time of loaded file
:- dynamic supress/1.      % Exclude terms from Python dictionary
:- dynamic param/4.        % param(Name, Type, Attr, Value)

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

sendLevel(1,Level) :-
    Level > 0,
    !,
    component(_, cellstat, Cellstat),
    send(Cellstat, l, Level),
    writeln(sentCellstatLevel(Level)).
sendLevel(Num,Level) :-
    Num > 1,
    Level > 0,
    LNum is Num-1,
    lagoon_number(Object, LNum),
    send(Object, l, Level),
    writeln(sentLagoonLevel(LNum, Level)).

send_info(end_of_file,_) :-
   write('Is debugging on? Possibly calling Cellstat '),
   writeln('level detection while working on Lagoons'),
   fail.
    
send_info(flux(F),Stream) :- !, newFlux(flux(F),Stream).

send_info(Levels,_) :-  % levels(Cellstat,L1,L2..)
    functor(Levels,levels,_),
    arg(Num,Levels,Level),
    sendLevel(Num,Level),
    fail.

send_info(Msg,_) :- writeln(send_info(Msg)).

% Code to launch python/OpenCV level detection programs

check_error(camera(IP))    :- writeln(error(camera(IP))),!,fail.
check_error(othererror(D)) :- writeln(error(othererror(D))),!,fail.
check_error(_).               % Everything else is not an error

get_level(Type) :-
    python(Python),
    evostat_directory(Dir),
    concat_atom([Dir,'levels.py'],LEVELS),
    CmdLine = [LEVELS,Type],
    ( retract(levelStream(Type,Previous)) ->
	catch( read(Previous, Info),
	       Ex,
	       (plog(caught(Ex,CmdLine)),sleep(1),fail)),
        check_error(Info),
	send_info(Info, Previous),
	catch( close(Previous), ExC, plog(caught(ExC,closing(python))))
     ; true
    ),
    evostat_directory(Dir),
    plog(launching(Python,CmdLine)),
    process_create(Python,CmdLine,
		   [stdout(pipe(Out)), stderr(std), cwd(Dir)]),
    assert(levelStream(Type,Out)),
    plog(launched),
    !.

get_level(Type) :- 
    plog(failed(levelUpdate(Type))).

newFlux(end_of_file,_) :- !.
newFlux(FluxTerm, Stream) :-
	FluxTerm =.. [Lagoon,FluxValue],
	send(@Lagoon, setFlux, FluxValue),
	catch(read(Stream, NextTerm),Ex,(plog(caught(Ex)),fail)),
	newFlux(NextTerm, Stream).

:- pce_begin_class(evostat, dialog, "PATHE Control Panel").
% Among other things, initializing this class asserts 'webok' semaphore    

initialise(W, Label:[name]) :->
          "Initialise the window and fill it"::
          send_super(W, initialise(Label)),
	  screen(WW,WH,DW,DH,Location),
	  EWidth is WW*DW/100,
	  EHeight is WH*DH/100,
          send(W, size, size(EWidth, EHeight)),
	  plog(evostat(width(EWidth),height(EHeight))),

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
    plog(frequentTimer(Frequent)),
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
         assert(webok),
	 maplist(create(@gui), Components),
	 initPID,                     % Start PID controllers
         send(@action?members, for_all,
	      if(@arg1?value==pIDon,message(@arg1, active, @off))),
         send(@action?members, for_all,
	      if(@arg1?value==pIDoff,message(@arg1, active, @on))),

         send(W,started),
         send_super(W, open, Location).

drain(_W, What) :->  plog(draining(What)).

stopped(_W) :->
       send(@ft,stop),  % Stop the GUI update timer as well
       plog('Stopping Level Detection (image processing)'),
       send(@ut, stop),
       send(@action?members, for_all,
	    if(@arg1?value==stop,message(@arg1, active, @off))),
       send(@action?members, for_all,
	    if(@arg1?value==start,message(@arg1, active, @on))),
       plog(stopped).

started(_W) :->
       plog('                      STARTING Level Detection'),
       send(@ut, start),
       send(@action?members, for_all,
	    if(@arg1?value==start,message(@arg1, active, @off))),
       send(@action?members, for_all,
	    if(@arg1?value==stop, message(@arg1, active, @on))),
       send(@ft,start),       % Now restart the fast GUI update timer
       plog('Starting Level Detection (Image processing)').

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
		   ( simulator -> plog(simulator)
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
    send(Self,quiet),      plog(sent(quiet)),
    send(Self,readLevels), plog(sent(readlevels)),
    send(Self,mixon),      plog(sent(mixon)),  % Send update
    prep, % refreshes assertion for Web page
    report,
    send(@gui, started).

quiet(Self) :->
    simulator -> true ;
    send(Self?graphicals, for_all,  % Lagoon mixers OFF
	 if(message(@arg1,instance_of,lagoon),message(@arg1,converse,'m0'))),
    component(_,cellstat,Cellstat),
    send(Cellstat,converse,'m0'),
    send(Cellstat,converse,'o-').

new_snapshot(Self) :->
    param(layout(Components)),
    memberchk(snapshot(Name, Position, Data), Components),
    new_component(@Name, snapshot, Data),
    send(@Name, slot, above, @x2),
    send(@Name, slot, below, @x1),
    send(@Name, slot, displayed, @on),
    send(@Name, slot, device, @gui),
    new(Graphicals,chain),
    send(Self?graphicals, for_all,
	 if( message(@arg1,instance_of,snapshot),
	     message(Graphicals,append,@Name,Position),
	     message(Graphicals,append,@arg1))),
    send(Self, slot, graphicals, Graphicals),
    plog(update(snapShot)).

mixon(Self) :->
    plog('Updating ebuttons'),
    send(Self?graphicals, for_all,
	 if(message(@arg1,instance_of,ebutton),message(@arg1,update))),
    plog('Updating snapshot'),
%    send(Self,new_snapshot),
    send(Self?graphicals, for_all,
	 if(message(@arg1,instance_of,snapshot),message(@arg1,update))),
    
    plog('Turning noisy stuff back on'),
    send(Self?graphicals, for_all,  % Lagoon mixers OFF
	 if(message(@arg1,instance_of,lagoon),message(@arg1,converse,'m1'))),
    plog('Lagoon mixers on'),
    component(_,cellstat,CellStat),
    send(CellStat,converse,'m1'),
    plog('CellStat mixer on'),
    send(CellStat,converse,'o2'),
    plog('Air on').
    
readLevels(_) :->
    plog(read_lagoon_levels),
    get_level(alllevels),
    plog(read_levels(finished)).

% Put things to be refreshed more often here:
% Image update, time to next level detection, etc.
% Currently only the autosampler/next cycle time indication

fastUpdate(_Self) :->
    change_request,
%    send(Self?graphicals, for_all,
%	 if(message(@arg1,instance_of,sampler),message(@arg1,fast_update))),
    check_web_files.

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
    retract(cycle(Last)),
    Next is Last + 1,
    assert(cycle(Next)),
    ( watcher(Who, Where, When),
      MyTime is Last mod When,
      MyTime =:= 0,
      concat_atom(['./smstext.py ',Where],Cmd),
      shell(Cmd),
      plog('                             TEXTING'(Who)),
      fail
    ; true
    ).
    
:- pce_end_class.  % End of evostat

% Initializers are extra arguments to the constructor
% Data is a list of messages to continue initializing the object

create(Dialog, Component) :-
	Component =.. [Type, Name, Position, Data],
        new_component(@Name, Type, Data),
	send(Dialog, append(@Name, Position)),
	retractall(component(Name,Type,_)),
        assert(component(Name,Type,@Name)).

new_component(@Name, Type, Data) :-
	free(@Name),
	Class =.. [Type,Name],
	new(@Name, Class),
	maplist(send(@Name), Data).

add_reset(Dialog,@Name) :-
    ( send(@Name,instance_of,ebutton) ->
	  plog(itsanebutton(@Name)),
	  new(Anon,reset('R',@Name)),
	  send(Dialog,append(Anon,right))
     ; plog(notsomuch(@Name))
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


% Create executable (saved-state) with goal [c],save_evostat.
% <name> of configuration is a command-line argument or <hostname>
% The corresponding Prolog file: <name>.pl,  must exist.
% Load config and generate Python .settings (dictionary) file.
% Many dynamic predicates (config, debug options ) from <hostname>.pl

update_config(Config) :-      % Load or reload when file changes
    config_name(Config,File),  %  Find configuration name (hostname or cmd arg)
    load_newest(Config,File).

load_newest(_,File) :-
    file_modtime(File, Time),                  % Mod time when File was loaded
    source_file_property(File,modified(Time)), % Matches!
    !.

load_newest(Config,File) :-
    plog(consulting(File)),
    consult(Config),
    plog(consulted(Config)),
    source_file_property(File,modified(Time)),
    retractall(file_modtime(File,_)),
    assert(file_modtime(File, Time)).

c :- main([]).

c(Name) :-
    free(@gui),
    new(@gui, evostat(Name)),
    send(@gui?frame, icon, bitmap('./evo.xpm')),
    prep,
    get(@gui, prompt, Reply),
    (Reply = quit ->
         send(@ft, stop),
         send(@ut, stop),
         send(@gui, destroy),
         stop_http
     ;   plog(Reply)
    ).

report :-
%    config_name(Root,_),
    open('evostat.report', append, S),
    nl(S), timeline(S),
    ( camera_exists -> true ; write(S,'NO CAMERA!'),nl(S) ),
    ( leak(Type)    -> write(S,'leak '),write(S,Type),nl(S) ; true ),
    reportTemperature(cellstat,S),
    reportTurbidity(cellstat,S),
    reportTemperature(lagoon,S),
    (err(Who,Err), write(S,error(Who,Err)),nl(S),fail ; true),
    close(S).

reportTemperature(What,S) :-
    component(Who, What, Obj),
    get(Obj,t,Val),
    HiC is integer(Val/10), LoC is integer(Val) mod 10,
    format(S, '~s Temp    ~d.~dC~n', [Who, HiC, LoC]),
    fail.
reportTemperature(_,_).

reportTurbidity(What,S) :-
    component(Who, What, Obj),
    get(Obj,b,ODVal),
    format(S, '~s OD600  .~d~n',[Who,ODVal]),
    fail.
reportTurbidity(_,_).

count(ErrorType, Who) :-
    Prev =.. [ErrorType, _When, HowMany],
    ( retract(err(Who,Prev)) -> NErrors is HowMany+1 ;  NErrors=1 ),
    timestring(Now),
    Result =.. [ErrorType, Now, NErrors],
    assert(err(Who,Result)).

main :-
    pce_main_loop(main). % This calls main(Argv)

main(_Argv) :-
    ( shell('./multiples',1)
     -> plog('EvoStat is already running'), halt
     ; assert(cycle(0))
    ),

    open('evostat.report', write, S),
    nl(S),write(S,'EvoStat started:'),timeline(S),close(S),
    
    evostat_directory(HomeDir),  % With this, the savestate
    cd(HomeDir),               % can be executed from anywhere
    assert(file_search_path(HomeDir)),
    cleanup,                   % Remove all temp_file/1 entries
    logging,                   % Send output to F if logfile(F) defined

	% Delay a bit if the computer is just starting up (low PID)
        current_prolog_flag(pid, PID),
        (PID < 900 -> sleep(30) ; true),

        set_prolog_flag(save_history,false),
	at_halt(pathe_report(verbose)),  % Special exit predicate to call
        load_bluetooth,
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
        start_http,
        c(Root),
        sleep(10),
        !,
	stop_http.

save_evostat :-
    os_emulator(Emulator),
%    retractall(debug),
    Options = [stand_alone(true), goal(main)],
    qsave_program(evostat, [emulator(Emulator)|Options]).

% SIGNAL HANDLING
% :- on_signal(int, _, cint).
%
% cint(_Signal) :- writeln('Caught a signal'),
%                  thread_send_message(main, cinter).

running   :- catch(thread_httpd:http_workers(21847,N),_,fail), N>0.

reload    :- stop_http, reconsult(webspec), start_http.

stop_http  :- catch(http_stop_server(21847,[]),_,true).

start_http :-
    ( running -> true
     ;
     process_files('web/*', [authentication(basic(pws,'Secure Page'))]),
%     process_files('web/*',        [] ),
     process_files('web/*.html', [] ),
     process_files('web/css/*',    [] ),
     process_files('web/js/*',     [] ),
     process_files('web/images/*', [] ),
     http_server( http_dispatch, [ port(21847) ] )
    ).

% Given a Chdir path of depth N,
% create a ladder back to original location

return_path(Path,Return) :-
    atom_chars(Path,PathChs),
    findall('/..', member('/',PathChs), Levels),
    concat_atom(['..'|Levels], Return).

newpipe(Name) :-
	concat_atom(['mkfifo ', Name], MakePipe),
	system(MakePipe).

% Run async command with stream output
run_external(Cmd, Stream) :-
	newpipe('/tmp/bpipe'),
	concat_atom([Cmd,' >/tmp/bpipe &'], Redirected),
	system(Redirected),
	open('/tmp/bpipe', read, Stream, []),
	system('rm /tmp/bpipe').

%http_read_passwd_file(+Path, -Data)
%http_write_passwd_file(pws,"$1$jVPltO5Q$$1$jVPltO5Q$t9a46Bb18vp/BMoco70u21")

change_request :-
  ( retract(changeRequest(List))
    -> maplist(new_value,List)
    ;  true % plog(no_changes)
  ).

% Make sure it is a number, remove leading decimal
ensure_value(Atom, Number) :-
        atom_codes(  Atom,   ACodes),
        ( ACodes = [0'.|Codes] -> true ; Codes=ACodes),
        number_codes(Number, Codes),
        !.
ensure_value(Atom, Atom).

% Send send <cmd><value> to object <compname>
 % when Attr has the form  <compname>_<cmd>

new_value(submit=_).

% Target Level settings for containers
new_value(Attr=Value) :-
  atomic_list_concat([Name,'lv'],'_',Attr), % Level setting command
  !,
  ensure_value(Value,EValue),
  send(@Name, targetLevel, EValue),
  plog(sent(@Name,targetLevel,EValue)).

new_value(Attr=Value) :-
  atomic_list_concat([Name,Cmd],'_',Attr),
  component(Name, _Type, Obj),
  ensure_value(Value,EValue),
  concat_atom([Cmd,EValue],Command),
  send(Obj,converse,Command),
  plog(sent(Obj,converse,Command)),
  change_value(Cmd, Name, EValue),  % Change the param and object value
  !.
new_value(I) :-  plog(failed(I)).


change_value('tt',Name,Value) :-
   !,
   component(Name, Type, Obj),
   retract(param(Name, Type, ttemperature, _)),
   assert(param(Name, Type, ttemperature, Value)),
   send(Obj, slot, ttemperature, Value).
change_value(_,_,_).

 
plog(Term) :- write(user_error,Term),nl(user_error).

logIP(Req) :-    
    memberchk(peer(IP),Req),
    open('ip.log', append, S),
    write(S,IP),nl(S),
    close(S).

get_background(ImageFile) :-
    gethostname(Fullname),
    atomic_list_concat([Name|_],'.',Fullname),
    concat_atom(['./images/',Name,'.png'],ImageFile).

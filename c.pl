% PROLOG FILES [c,gbutton,adjust,newpid,iface,dialin,prephtml,webspec,util].
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

:- set_prolog_flag(double_quotes,codes).
:- use_module(library(lists), [ flatten/2 ] ).
%:- use_module(library(lists)).
:- use_module(library(time)).
:- use_module(library(pce)).
:- use_module(library(process)). % For external programs
:- use_module(library(charsio)).
:- use_module(library(helpidx)).
:- use_module(library(ctypes)).

:- ensure_loaded(util).    % EvoStat Utilities
:- ensure_loaded(webspec). % HTTP Request handlers
:- ensure_loaded(et).      % Term Expansion for XPCEgen

evostat_directory(Dir) :-  working_directory(Dir,Dir).

:- free(@'_dialog_bg'),  % This should make the background light-blue steel
   XR is 0xB0*257, XG is 0xC4*257, XB is 0xDE*257,
   new(@'_dialog_bg', colour(@default,XR,XG,XB)).

:- dynamic [ component/3,
	     config/1,       % FILES  config info
	     file_modtime/2, % Loaded file modification time
	     logfile/1,      %

             leak/1,       % Reporting
	     textCycle/1,
	     bt_device/2,
	     watcher/2,
	     err/2,

	     webok/0,  % Assert when Web info is available
	     screen/5,
	     levelStream/2,
	     air/0,
	     mix/0,
	     simulator/0,
	     changeRequest/1, % HTML form values
	     toggle_auto/0 ].

% List of temporary files for cleanup
temp_file('mypic1.jpg').
temp_file('mypic2.jpg').
temp_file('dbg.txt').
temp_file(File)       :- logfile(File).

%%%%%%%%%%%%%%%% TIMERS
timer(autoUpdate,  Seconds) :-	param(updateCycle(Seconds)).
timer(fastUpdate,  Fast)    :-	param(updateCycle(UC)), Fast is UC/10.
timer(sendTexts,  Seconds)  :-  param(textMessages(Seconds)).

%%%%%%%%%%%%%%%% CAMERA
camera_device(Device) :-
    param(camera(Num)),
    integer(Num),
    !,
    concat_atom(['/dev/video',Num],Device).

camera_exists :-
    camera_device(Device),
    access_file(Device,exist).
    
camera_reset :- windows, !.   % Null camera reset on Windows
camera_reset :- \+ camera_exists,  % No camera to reset
		plog(no_camera),
		!.
camera_reset :-
    camera_device(Device),
    evostat_directory(Dir),
    Cmd = '/usr/bin/uvcdynctrl',
    config_name(Config,_),    % Hostname or evostat argument
    concat_atom([Config,'.gpfl'], Settings),
    concat_atom(['--device=',Device],Option),
    Args = ['-L', Settings, Option],
    plog(resettingCamera(Dir, Cmd, Args)),
%    open('/dev/null', write, Null),
    process_create(Cmd,Args,[cwd(Dir)]),
    plog(resetCamera).
%%%%%%%%%%%%%CAMERA
%% SUBSYSTEMS
:- [gbutton]. % XPCE parent class [ebutton] etc.
:- [adjust].  % PID controller <-> PCE interface

% Grammar to convert Prolog term to Python dictionary
:- dynamic tabs/1.

writePythonParams(EvoStat) :-    
    config(List),
    findall(H, ( member(H, List),
                 functor(H, F, A),
                 \+ memberchk(F/A,[layout/1,screen/3])), % Remove
            PyTerms),
    assert(tabs(1)),
    pl2py(PyTerms, PythonDict, []),
    flatten(['{\n',PythonDict,'}\n'], Flat),
    concat_atom(Flat, PyAtom),
    concat_atom([EvoStat,'.settings'],File), % Write it out
    tell(File),
    write(PyAtom),
    told,
    current_prolog_flag(argv,Args),
    ( memberchk(gen,Args) -> halt ; true ).

% DCG compatible version of univ simplifies syntax
'=..'(Term,List,T,T) :- Term =.. List.

quote_atom(false) --> !, ['None'].  % Python's false = None
quote_atom([A])   --> !, quote_atom(A).
quote_atom(N)     --> { number(N)}, !, [N].
quote_atom(A)     --> { atom(A) }, ['''',A,''''].

quote_atoms([],_)         --> [].
quote_atoms([A],_)        --> quote_atom(A),!.
quote_atoms([A|T],Spacer) --> quote_atom(A), [Spacer], quote_atoms(T,Spacer).

tabin  --> { retract(tabs(N)), NN is N + 1, assert(tabs(NN)) }.
tabout --> { retract(tabs(N)), NN is N - 1, assert(tabs(NN)) }.

indent     --> { tabs(N) }, indent(N).
indent(N)  --> { N<1},  !.
indent(N)  --> { N>0, NN is N-1}, ['    '], indent(NN).


pl2py([])    --> !, [].
pl2py(A)     --> quote_atom(A), !.
pl2py([H])   --> !, indent, pl2py(H), [' \n'].
pl2py([H|T]) --> !, indent, pl2py(H), pl2py(T).

pl2py(Term) --> Term =.. [F,A],   % SINGLE f(a)
	        quote_atom(F),
		[' :  '],
	        quote_atom(A),
		!,
		[',\n'].

pl2py(Term) --> Term =.. [F,A|As],    % TUPLE   f(a,b,...)
                !,
	        quote_atom(F),
	        [' : ('],
	        quote_atoms([A|As], ','),
                [ '),\n' ].

pl2py(Term) --> Term =.. [F|Args],  % Nested
	        quote_atom(F),
		[' :  {'],
		pl2py(Args),
		['      }'].
                 
% Additional messages: pathe_report/1 called on exit.
% Call append/1 because output logfile has been closed.

pathe_report(_) :- plog(final_report),fail.

pathe_report(verbose) :-
    logfile(File),
    append(File),
    writeln(verbose_test_report1),
    told.

pathe_report(moderate) :-
    logfile(File),
    append(File),
    writeln(moderate_test_report),
    told.

pathe_report(_) :-
    plog('no_logging').

%
% Find the Nth lagoon(object), names can be complex
% but must end with a single digit (usually 1-4)
% E.g. lagoonDarwin2, lagoon_huxley_1, cuvierVirustat4
%
component_index(Object, 0) :- component(_, cellstat, Object),!.
component_index(Object, N) :- component(Lagoon, lagoon, Object),
			      atom_codes(Lagoon, Codes),
			      append(_,[Digit],Codes),
			      N is Digit + 0'0.

send_info(end_of_file,_) :-
   plog('Is debugging on? Possibly called Cellstat'),
   plog('level detection while working on Lagoons'),
   fail.
    
send_info(flux(F),Stream) :- !, newFlux(flux(F),Stream).

send_info(Levels,_) :-  % levels(Cellstat,Lagoon1,L2..)
    Levels =.. [levels|Ls],
    send_levels(Ls,0).

send_info(Msg,_) :- writeln(send_info(Msg)).

% Levels from OpenCV/python camera program
% are stored in object l[evel] variable here.
% levels(0:cellstat, 1:lagoon1, 2:lagoon2, etc.)

send_levels( [],   _).
send_levels([L|Ls],N) :-
    component_index(Obj,N),
    send(Obj,l,L),
    NN is N + 1,
    send_levels(Ls,NN).

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
	  findall(Tm,make_timer(W, Tm), _Tms),  % Make the Timers
	  send(MB, append, new(File, popup(file))),
          free(@action),
          ( current_prolog_flag(argv,[Exe|_]),
	    atom_concat(_,'evostat',Exe)
	   -> DBOK = []
            ; DBOK = [menu_item(ok,message(W, ok))] % Back to ?- prompt
	  ),
	  send_list(File, append,[menu_item(quit,message(W, quit))|DBOK] ),
	  send(MB, append, new(@action, popup(action))),
	  send_list(@action, append,[ menu_item('Drain Lagoons',
					message(W, drain, lagoons)),
				    menu_item('Drain Cellstat',
				        message(W, drain, cellstat)),
				    menu_item('no update',
					      message(W, stopped)),
				    menu_item(update,
					      message(W, started)),
				    menu_item('no pid',
					      message(W, stopPID)),
				    menu_item(pid,
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
	 initPID,                        % Start PID controllers
         send(@action?members, for_all,
	      if(@arg1?value==pIDon,message(@arg1, active, @off))),
         send(@action?members, for_all,
	      if(@arg1?value==pIDoff,message(@arg1, active, @on))),

         send(W,started),
         send_super(W, open, Location),
	 plog(finished(evostat)).


% make_timer(?,?)
make_timer(W, Name) :-
    timer(Name,Seconds),
    new(M, message(W, Name)),  % Create Message for Timer Object
    concat_atom([Name,timer],TName),
    free(@TName),
    send(W, attribute, attribute(timer, new(@TName, timer(Seconds, M)))).

drain(_W, What) :->  plog(draining(What)).

stopped(_W) :->
       send( @fastUpdatetimer, stop),  % Stop fast (GUI) updates
       plog('Stopping Level Detection (image processing)'),
       control_timer(autoUpdate, stop),
       plog(stopped).

started(_W) :->
       plog('                      STARTING Level Detection'),
       control_timer(autoUpdate, start),
       send(@fastUpdatetimer,start),   % Restart GUI updates
       plog('Starting Level Detection (Image processing)').

stopPID(_W)  :-> pidstop,  ghost_state(pid,stop).
startPID(_W) :-> pidstart, ghost_state(pid, start).

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
        send(W, return(File)).

ok(W) :->
        "User pressed the Ok button"::
        send(W, return(ok)).

prompt(W, Value:name) :<-
        "Open it, destroy it and return the result"::
        get(W, confirm, Value).

% Updating can take a while, so we must:
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
% COMPONENT UPDATES IN MIXON
    send(Self,mixon),      plog(sent(mixon)),  % Send update
    prep,              % refreshes assert with Web page data

    param(updateCycle(Seconds)),
    retractall(next_update(_)), % Reset the count-down
    assert(next_update(Seconds)),
    
    report,
    send(@gui, started).

quiet(Self) :->
    simulator -> true ;
    send_to_type( Self?graphicals, lagoon,  [converse,m0] ), % Mixers OFF
    send_to_type( Self?graphicals, cellstat, [converse,m0] ), % Mixers OFF
    send_to_type( Self?graphicals, cellstat, [converse,'o-'] ). % Air OFF

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
    plog('Updating all ebuttons'),
    send_to_type(Self?graphicals, ebutton, [update]),
    plog('Updating snapshot'),
    send_to_type(Self?graphicals, snapshot, [update]),
    plog('Turning noisy stuff back on'),
    send_to_type( Self?graphicals, lagoon, [converse,m1] ), % Mixers ON
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

% Things to be refreshed often (e.g. GUI) such as
% the Next Update countdown in Sampler label.

fastUpdate(Self) :->
    change_request,
    retract(next_update(Seconds)),
    Next is Seconds - 10,
    assert(next_update(Next)),
    send_to_type(Self?graphicals, sampler, [up,Next]),
    send_to_type(Self?graphicals, sampler, [update]),
    prep,
    check_web_files.

sendText(Self) :->
    send(Self, sendTexts), % Send first text message
    control_timer(texting, start).

stopText(_S)   :->
    control_timer(texting, stop).
    
sendTexts(_Self) :->
    retract(textCycle(Now)),
    Next is Now + 1,
    assert(textCycle(Next)),
    findall(_,sending_text(Now),_).

sending_text(Now) :-
    watcher(_Who, Where, When),
    0 is Now mod When,
    concat_atom(['./smstext.py ',Where], Cmd),
    shell(Cmd).
    
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

% Bluetooth interface, with error checking and fail messages
% bluetalk(+Socket, +Cmd, -Reply).

bluetalk(@nil,  _,  'no_connection.'  ).
bluetalk(   _,  '', 'nothing_to_send.').
bluetalk( S, Cmd, Reply) :- bt_converse(S ,Cmd, Reply).
bluetalk(   _,   _, 'send_failed.'    ).

% Create executable (saved-state) with:  [c],save_evostat.
%
% The NAME of EvoStat is command-line argument or <hostname>
%
% The configuration NAME.pl, must exist in working directory
% (Copy template.pl and then edit for your configuration)
% 
% Load config and generate Python (dict) NAME.settings

update_config(Config) :-      % Load/reload when file changes
    config_name(Config,File), % Config is hostname or Argv[1]
    load_newest(File).

c :- main([]),!. % This calls c(EvoStatName) (choicepoint somewhere?)

c(Name) :-
    free(@gui),
    new(@gui, evostat(Name)),
    send(@gui?frame, icon, bitmap('./evo.xpm')),
    prep,     % Initial data for web pages
    assert(webok),
    start_http,
    get(@gui, prompt, Reply),
    (Reply = quit ->
         send(@fastUpdatetimer, stop),
         send(@autoUpdatetimer, stop),
         send(@gui, destroy),
	 stop_http,
	 plog(http(stopped)),
	 halt % c/1 called at end of main/1, so halt is okay.
     ;   plog(Reply) % GUI and webserver still runnning
    ),
    plog(c(done)).

report :-
    open('evostat.report', append, S),
    nl(S), timeline(S),
    ( camera_exists -> true ; write(S,'NO CAMERA!'),nl(S) ),
    ( leak(Type)    -> write(S,leak(Type)),nl(S) ; true ),
    reportTemperature(cellstat,S),
    reportTurbidity(cellstat,S),
    findall(_,reportTemperature(lagoon,S),_),
    findall(_,(err(Who,Err),write(S,error(Who,Err)),nl(S)),_),
    close(S).

reportTemperature(What,S) :-
    component(Who, What, Obj),
    get(Obj,t,Val),
    HiC is integer(Val/10), LoC is integer(Val) mod 10,
    format(S, '~s Temp    ~d.~dC~n', [Who, HiC, LoC]).

reportTurbidity(What,S) :-
    component(Who, What, Obj),
    get(Obj,b,ODVal),
    format(S, '~s OD600  .~d~n',[Who,ODVal]).

evostat_running :-
    shell('./multiples',1),
    plog('EvoStat is already running'),
    halt.

main(_Argv) :-
    \+ evostat_running,
    assert(textCycle(0)),
    open('evostat.report', write, S),
    nl(S),write(S,'EvoStat started:'),timeline(S),close(S),
    evostat_directory(HomeDir),
    assert(file_search_path(HomeDir)),
    cleanup,     % Remove temp_file/1 entries
    logging,     % stderr to FILE if logfile(FILE)

    % Delay if the computer is just starting up (low PID #)
    current_prolog_flag(pid, PID),
    (PID < 900 -> sleep(30) ; true),
    set_prolog_flag(save_history,false),
    at_halt(pathe_report(verbose)),  % Called on exit
    ( windows
      -> load_foreign_library(foreign(plblue))
      ;  load_foreign_library(plblue)
    ),
    plog(loaded(bluetooth)),
    update_config(Root),
    param(screen(WF,HF,Loc)),    % From configuration data
    get(@display?size,width,Width),
    get(@display?size,height,Height),
    assert(screen(Width,Height,WF,HF,Loc)),
    camera_reset,
    param(layout(Components)),
    Layout =.. [Root,Components],
    assert(Layout),
    writePythonParams(Root),
    c(Root).

% From Web Form submission:  Attr=Value =>  <name>_<var>=<value>
% Results in sending <var><value> to object with <name>
change_request :-
    ( changeRequest(List)
    -> maplist(new_value,List),
       retract(changeRequest(List))
    ; true
    ).

new_value(Attr=Value) :-
  atomic_list_concat([Name,Var],'_',Attr),
  ensure_value(Value, EValue), % atom -> int if possible
  component(Name, _, Obj),
  get(Obj, Var, OldValue),
  ( OldValue == EValue
   -> plog(unchanged(Name,Var))
   ;  send(Obj, Var, EValue),
      assert(changed(Obj,Var))
  ).

backgroundImage(ImageFile) :-
    config_name(Name,_),
    concat_atom(['./images/',Name,'.png'],ImageFile).

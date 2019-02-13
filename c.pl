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
:- use_module(library(process),[process_create/3]).
:- use_module(library(charsio)).
:- use_module(library(helpidx)).
:- use_module(library(ctypes)).

:- expects_dialect(sicstus).
:- ensure_loaded(library(system)).

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
	     motd/1,
	     last_mouse/1,

             leak/1,       % Reporting
	     textCycle/1,
	     bt_device/2,
	     watcher/3,
	     err/2,
	     web_control/1,    % Control: Restart evostat,X11,etc.

	     webok/0,  % Assert when Web info is available
	     webValue/3,
	     screen/5,
	     levelStream/2,
	     childProcess/4,
	     level/3,
	     air/0,
	     mix/0,
	     simulator/0,
	     changeRequest/1, % HTML form values
	     changed/3,       % push (Obj,Var) to Arduino
	     toggle_auto/0 ].
:- multifile motd/1.

% List of temporary files for cleanup
temp_file('mypic1.jpg').
temp_file('mypic2.jpg').
temp_file('dbg.txt').
temp_file(File)       :- logfile(File).

%%%%%%%%%%%%%%%% TIMERS
timer(update,  Seconds) :-	param(updateCycle(Seconds)).
timer(fastUpdate,  Fast)    :-	param(updateCycle(UC)), Fast is UC/10.
timer(texting,     Seconds)  :-  param(textMessages(Seconds)).

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
camera_reset :- !.
camera_reset :-
    camera_device(Device),
    evostat_directory(Dir),
    Cmd = '/usr/bin/uvcdynctrl',
    config_name(Config,_),    % Hostname or evostat argument
    concat_atom([Config,'i.gpfl'], Settings),
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
    ( memberchk(gen,Args) -> close(user_error), halt ; true ).

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
% index_valve(Index,In,Out).
index_valve(0,v0,v0).
index_valve(1,v1,v1).
index_valve(2,v1,v2).
index_valve(3,v1,v3).
index_valve(4,v1,v4).

component_index(Object, 0) :- component(_, cellstat, Object),!.
component_index(Object, N) :- component(Lagoon, lagoon, Object),
			      atom_codes(Lagoon, Codes),
			      append(_,[Digit],Codes),
			      N is Digit - 0'0.

send_info(end_of_file,_) :-
   plog('Is debugging on? Possibly called Cellstat'),
   plog('level detection while working on Lagoons'),
   fail.
    
send_info(flux(F),Stream) :- !, newFlux(flux(F),Stream).

send_info(Levels,_) :-  % levels(Cellstat,Lagoon1,L2..)
    Levels =.. [levels|Ls],
    dblog(pid,Levels),
    report_temperatures(_),
    send_levels(Ls,0).

send_info(Msg,_) :- writeln(send_info(Msg)).

component_temperature(Name,Type,Temp) :-
    component(Name,Type,Obj),
    get(Obj,t,Value),
    High is integer(Value/10), Low is integer(Value) mod 10,
    format(atom(Temp), '~s ~d.~d C',[Name, High, Low ]).

report_temperatures(TLine) :-
    component(CName,cellstat,Co),
    get(Co,t,CVal),
    CHiC is integer(CVal/10), CLoC is integer(CVal) mod 10,
    LName = lagoon2, % hack
    component(LName,lagoon,Lo),
    get(Lo,t,LVal),
    LHiC is integer(LVal/10), LLoC is integer(LVal) mod 10,
    dblog(pid,temps(CVal,LVal)),
    format(atom(TLine), '~s ~d.~d C ~s ~d.~d C',
	        [CName, CHiC, CLoC, LName, LHiC, LLoC ]).
    
% Levels from OpenCV/python camera program
% are stored in object l[evel] variable here.
% levels(0:cellstat, 1:lagoon1, 2:lagoon2, etc.)

send_levels( [],   _).
send_levels([L|Ls],N) :-
    component_index(Obj,N),
    component(Who,_,Obj),
    plog(sending(l(L),Who)),
    send(Obj,l,L),
    NN is N + 1,
    plog(nextWillBe(Ls,NN)),
    send_levels(Ls,NN).

% Code to launch python/OpenCV level detection programs

check_error(camera(IP))    :- writeln(error(camera(IP))),!,fail.
check_error(othererror(D)) :- writeln(error(othererror(D))),!,fail.
check_error(_).               % Everything else is not an error

% Same as set_prolog_flag(unknown, fail), but safer (abolish does this!)
safe_childProcess(A,B,C,D) :-
    safe(childProcess(A,B,C,D),fail).

%!  launch(+Atom) is semidet.
% This will destroy any existing child of the same Program
launch(Program) :-
    plog(launch_prep),
    ( safe(childProcess(Program,PPID,PIn,_POut), fail) ->
	 plog(child(Program, PPID)),
	 catch(nl(PIn),Ex,
	       (plog(launchEx(Ex)),
		get_prolog_backtrace(20,BT),
		print_prolog_backtrace(user_error,BT,[subgoal_positions(true)]))),
	 plog(before(flush)),
	 catch(flush_output(PIn),Fex,(plog(flush_ex(Fex)),true)),
	 plog(before(wait)),
	 catch(process_wait(PPID, Return),Wex,
		     (plog(wait_ex(Wex)),Return=wait_failed)),
	 sleep(1), % Make sure camera is free
	 plog(launch_preparation(Program, Return))
     ; plog(no_safe_child) % , true
    ),
    launch0(Program).

launch(Program) :- plog(launch_failed(Program)),fail.

%!  launch0(+Atom) is det.

launch0(Program) :- % First time, or after recovery
    retractall(childProcess(Program,_,_,_)),
    python(Python),
    evostat_directory(Dir),
    concat_atom([Dir,Program,'.py'],LEVELS),
    CmdLine = [LEVELS],
    plog(launch0),
    process_create(Python, CmdLine,
		   [stdout(pipe(Out)), stdin(pipe(In)),
		    cwd(Dir),
		    process(PID)
		   ]),
    plog(launched(PID)),
    assert(childProcess(Program,PID,In,Out)).

safe_get_level(Program) :-
    catch( get_level(Program), Ex,
	   (plog(relaunching(Program,Ex)),
	    childProcess(Program,PID,In,Out),
	    catch(close(In),InE,plog(close(InE))),
	    catch(close(Out),OutE,plog(close(OutE))),
	    catch(process_wait(PID,_),WaitE,plog(wait(WaitE))),
	    plog(caught_launch),
	    launch(level))).

get_level(Program) :-
    ( safe_childProcess(Program,_PID, In, Out)
     -> ( report_temperatures(TempData);TempData='No Temperature Data'),
	  plog(TempData),
	  writeln(In,TempData),
	  flush_output(In),
	  repeat,  % Can get trapped here!!!
	      set_stream(Out,timeout(20)),
	      catch( read(Out, Term), _, (close(In),
					  close(Out))),
	      ( Term = end_of_data -> true
		; functor(Term, F, A),
		  abolish(F/A),
		  assert(Term),
		  propagate(Term),
		  fail
	      ),
	      !
     ; plog(failed(levelUpdate(Program)))
     ).

newFlux(end_of_file,_) :- !.
newFlux(FluxTerm, Stream) :-
	FluxTerm =.. [Lagoon,FluxValue],
	send(@Lagoon, setFlux, FluxValue),
	catch(read(Stream, NextTerm),Ex,(plog(caught(Ex)),fail)),
	newFlux(NextTerm, Stream).

:- pce_begin_class(evostat, dialog, "PATHE Control Panel").

variable(movement, real, both, "Last User Interaction").

initialise(W, Label:[name]) :->
          "Initialise the window and fill it"::
          send_super(W, initialise(Label)),
	  screen(WW,WH,DW,DH,Location),
	  EWidth is WW*DW/100,
	  EHeight is WH*DH/100,
          send(W, size, size(EWidth, EHeight)),
	  plog(evostat(width(EWidth),height(EHeight))),
	  get_time(Time),
	  send(W, slot, movement, Time),
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
	  send_list(File, append,[menu_item(reload,message(W,reload)),
				  menu_item(quit,message(W, quit))|DBOK] ),
	  send(MB, append, new(@action, popup(action))),
	  send_list(@action, append,[ menu_item('Drain Lagoons',
					message(W, drain, lagoons)),
				    menu_item('Drain Cellstat',
				        message(W, drain, cellstat)),
				    menu_item(update,
					      message(W, started)),
				    menu_item('no update',
					      message(W, stopped)),
				    menu_item(pid,
					      message(W, startPID)),
				    menu_item('no pid',
					      message(W, stopPID)),
				    menu_item(texting,
					      message(W, texting)),
				    menu_item('no texting',
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
	 newmotd,
	 setup_web_values,
	 initPID,                        % Start PID controllers
         send(@action?members, for_all,
	      if(@arg1?value==pIDon,message(@arg1, active, @off))),
         send(@action?members, for_all,
	      if(@arg1?value==pIDoff,message(@arg1, active, @on))),

         send(W,started),
         send_super(W, open, Location),
	 plog(normal(launch)),
	 launch(level),
	 plog(finished(evostat)).

newmotd :-
     timeAtom(Now),
     ( motd(MOTD) -> true ; MOTD = '-' ), % From <hostname>.pl
     concat_atom([MOTD, ' Restarted ', Now], Message),
     retractall(motd(_)),
     assert(motd(Message)).

event(Self, Ev:event) :->
     send(Ev, is_a, mouse),
     get_time(Time),
     send(Self,slot, movement, Time),
     send_super(Self, event(Ev)).

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
       plog('        Stopping AUTO update timer to perform UPDATE'),
       control_timer(update, stop),
       plog(stopped).

started(_W) :->
       control_timer(update, start),
       send(@fastUpdatetimer,start),   % Restart GUI updates
       plog('        Re-starting AUTO Update/Level Detection timer').

reload(_W) :-> stop_http, reconsult(webspec), start_http.

stopPID(_W)  :-> pidstop,  ghost_state(pid,stop).
startPID(_W) :-> pidstart, ghost_state(pid, start).

cellstat(_W) :-> "User pressed the CellStat button"::
                  plog('User pressed CellStat').

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

statistics(_) :- !.
statistics(ID) :-
    get(class(object), no_created, @on, Created),
    get(class(object), no_freed, @on, Freed),
    Current is Created - Freed,
    plog('__________________objects'(Created, Freed, ID, Current)).

    
update(Self) :->
    get(Self,name,ClassName),
    plog(update(ClassName)),
    send(@gui, stopped),
    update_config(_),      plog(updated(config)),
    check_condition(leak), % Immediately send text if leak detected
    send(Self,quiet),      plog(sent(quiet)),
    send(Self,readLevels), plog(sent(readlevels)),
    % COMPONENT UPDATES IN MIXON
%    plog(updatingall),
    send(Self,mixon),
    prep,              % refreshes assert with Web page data 
    param(updateCycle(Seconds)),
    retractall(next_update(_)), % Reset the count-down
    assert(next_update(Seconds)),
%    plog(calling(report)),
    report,
    send(@gui, started),
    catch( datalog, Ex, plog(exception(Ex))),
    plog(start(gui)).

update(Self) :->
      get(Self,name,ClassName),
      plog(update(ClassName,failed)).

quiet(Self) :->
    send_to_type( Self?graphicals, lagoon,  [converse,m0] ), % Mixers OFF
    send_to_type( Self?graphicals, cellstat, [converse,m0] ), % Mixers OFF
    send_to_type( Self?graphicals, cellstat, [converse,'o-'] ), % Air OFF
    !.
quiet(Self) :->
    get(Self,name,ClassName),
    plog(quiet(ClassName,failed)).

new_snapshot(_Self) :->
	    plog('Calling new_snapshot: should not happen').
%    param(layout(Components)),
%    memberchk(snapshot(Name, Position, Data), Components),
%    new_component(@Name, snapshot, Data),
%    send(@Name, slot, above, @x2),
%    send(@Name, slot, below, @x1),
%    send(@Name, slot, displayed, @on),
%    send(@Name, slot, device, @gui),
%    new(Graphicals,chain),
%    send(Self?graphicals, for_all,
%	 if( message(@arg1,instance_of,snapshot),
%	     message(Graphicals,append,@Name,Position),
%	     message(Graphicals,append,@arg1))),
%    free(Self?graphicals),
%    send(Self, slot, graphicals, Graphicals),
%    plog(update(snapShot)).

mixon(Self) :->
    statistics(a),
    send_to_type(Self?graphicals, ebutton, [update]),
    statistics(b),
    get_time(Now),
    get(Self, movement, MouseMoved),
    Elapsed is Now - MouseMoved,
    (Elapsed < 100
     -> send_to_type(Self?graphicals, snapshot, [update])
     ; plog(no_snapshot_update(Elapsed))
    ),
    plog('Turning noisy stuff back on'),
    statistics(c),
    send_to_type( Self?graphicals, lagoon, [converse,m1] ), % Mixers ON
    send_to_type( Self?graphicals, cellstat, [converse,m1] ), % Mixers ON
    send_to_type( Self?graphicals, cellstat, [converse,'o2'] ), % Air ON
    plog('Cellstat mixer, Lagoon mixers, Air on').
    
readLevels(_) :->
	  safe_get_level(level),
          !.

readLevels(_) :->
    plog(readLevels(failed)).

%    catch( consult_python('./alevel.py', [], 2),
%	   Caught,
%	   plog(exception(consult_python/2, Caught)) ),
%    findall(level(A,B,C,D),level(A,B,C,D),Levels),
%    plog(levels(Levels)),
%    findall(Who:Level, propagate_level(Who,Level),Props),
%    plog(propagated(Props)),
%    flow_report(Levels)
%    ).
    
% readLevels(_) :-> get_level(alllevels).

% Things to be refreshed often (e.g. GUI) such as
% the Next Update countdown in Sampler label.

fastUpdate(Self) :->
    check_web_control,
    retract(next_update(Seconds)),
    Next is Seconds - 10,
    assert(next_update(Next)),
    send_to_type(Self?graphicals, sampler, [up,Next]),
%    plog(updated(next_update,Next)),
    send_to_type(Self?graphicals, sampler, [update]),
    prep,
    check_web_files,
%    plog(fastUpdate(finished)),
    !.

fastUpdate(_) :-> plog(fastUpdate(failed)).

texting(Self) :->
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
    online,
    watcher(_Who, Where, When),
    0 is Now mod When,
    evostat_directory(Dir),
    concat_atom(['/usr/bin/python ',Dir,'smstext.py ',Where], Cmd),
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
        open('evostat.about', read, Stream),
	read_pending_input(Stream,FileContent,[]),
	close(Stream),
	atom_chars(About,FileContent).

% Bluetooth interface, with error checking and fail messages
% bluetalk(+Socket, +Cmd, -Reply).

bluetalk(@nil,  _,  no_connection  ).
bluetalk(   _,  '', nothing_to_send).
bluetalk( S, Cmd, Reply) :- bt_converse(S ,Cmd, Reply),!.
bluetalk(   _,   _, send_failed    ).

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

printpair(X,Y) :- plog(bluetooth(X,Y)).
    
scan :-
    load_foreign_library(plblue),
    bt_scan(A,B),
    maplist(printpair,A,B).

c :- main([]),!. % This calls c(EvoStatName) (choicepoint somewhere?)

c(Name) :-
    ( exists_directory('/tmp/timelapse') -> true
    ; make_directory('/tmp/timelapse')
    ),
    free(@gui),
    new(@gui, evostat(Name)),
    send(@gui?frame, icon, bitmap('./evo.xpm')),
    prep,     % Initial data for web pages
    assert(webok),
    start_http,
    % It might be a while before first sampler communication
    % So go ahead and try connecting Bluetooth(TM)
    send_to_type(@gui?graphicals, sampler, [connect]),
    get(@gui, prompt, Reply),
    (Reply = quit ->
         send(@fastUpdatetimer, stop),
         send(@updatetimer, stop),
         send(@gui, destroy),
	 stop_http,
	 plog(http(stopped)),
	 halt % c/1 called at end of main/1, so halt is okay.
     ;   plog(Reply) % GUI and webserver still runnning
    ),
    plog(c(done)).

initial_sync :-
    setof(Name:Vs,Type^Obj^( component(Name,Type,Obj),
			     setof(V,A^B^( component_valves(Name,V,A,B)
	 				  ;component_valves(A,B,Name,V)),Vs) ),
	  All ),
    initial_sync_list(All).

initial_sync_list([]).
initial_sync_list([Name:Vs|T]) :-
    send(@Name, connect),
    maplist(send(@Name,pull),Vs),
    initial_sync_list(T).
    

initialize_report :-
    open('evostat.report', write, S),
    nl(S), write(S,'EvoStat started:'),
    timeline(S),
    close(S).

report :-
    open('evostat.report', append, S),
    nl(S), timeline(S),
    ( camera_exists -> true ; write(S,'NO CAMERA!'),nl(S) ),
    report_conditions(S),
    reportTemperature(cellstat,S),
    reportTurbidity(cellstat,S),
    findall(_,reportTemperature(lagoon,S),_),
    findall(_,(err(Who,Err),write(S,error(Who,Err)),nl(S)),_),
    close(S),
    !.
report :- plog(report(failed)).

report_conditions(S) :-
    reported(Condition,Type,_,Value),
    write(S,status(Condition,Type,Value)),nl(S),
    fail.
report_conditions(_).

reportTemperature(What,S) :-
    component(Who, What, Obj),
    get(Obj,t,Val),
    HiC is integer(Val/10), LoC is integer(Val) mod 10,
    format(S, '~s Temp    ~d.~dC~n', [Who, HiC, LoC]),
    dblog(temperature,temperature(Who,Val)).

reportTurbidity(What,S) :-
    component(Who, What, Obj),
    get(Obj,b,ODVal),
    format(S, '~s OD600  .~d~n',[Who,ODVal]).

setup_web_values :-
    findall(T,web_values(T),Ts),
    plog(setup_web_values(Ts)).
    
web_values(lagoon) :-
    component(_C, lagoon, Obj),
    assert(webValue(Obj, l,0)),
    assert(webValue(Obj,tt,0)),
    assert(webValue(Obj,tl,0)),
    assert(webValue(Obj,tf,0)).
web_values(cellstat) :-
    component(_, cellstat, Obj),
    assert(webValue(Obj, l,0)),
    assert(webValue(Obj,tt,0)),
    assert(webValue(Obj,tb,0)),
    assert(webValue(Obj,tl,0)),
    assert(webValue(Obj,tf,0)).

evostat_running :-
    shell('./multiples',1),
    plog('EvoStat is already running'),
    halt.

% Install:   sudo ln -s /home/pi/src/EvoStat/evostat /usr/local/bin/evostat
%
% From the symbolic link, we can locate the EvoStat directory

evostat_location(Location) :-
    popen('/usr/bin/dirname \`readlink -f \\\`which evostat\\\`\`',read,S),
    read_stream_to_codes(S,Codes),
    append(Front,[_NL],Codes),
    atom_codes(Location,Front),
    working_directory(_,Location).

main(Argv) :-
    evostat_location(Where), % Find and move to EvoStat directory
    ( memberchk(where,Argv) -> writeln(Where), halt ; true ),
    \+ evostat_running,
    assert(textCycle(0)),
    initialize_report,
    evostat_directory(HomeDir),
    assert(file_search_path(HomeDir)),
    cleanup,     % Remove temp_file/1 entries
    logging,     % stderr to FILE if logfile(FILE)

    % Delay if the computer is just starting up (low PID #)
    current_prolog_flag(pid, ProcessID),
    (ProcessID < 900 -> sleep(30) ; true),
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

% Clear and Execute all Web Controls
% Web Controls are functions that cannot be executed by the HTTP server
% System functions, such as evostat or X11 restart and
% XPCE Messaging such as changing parameters in microcontrollers.

% The HTTP subsystem has no access to the file system or priveledged commands.
% Therefore, it asserts web_control(Request)
% If Request is either:
%  A predicate to be called
%  A filename which will be touched (brought into existence)
% 
% A priviledged process is watching for the presence of such 'signal' files
% and will take the appropriate action.
% From kmd.pl :
% [ levelrestart/0, evostatrestart/0, Predicates called from here
%   xrestart and linuxrestart are handled in by crontab (system_actions.sh)
%

levelrestart :- launch(level), % Should clean up any old instance of process
		plog(relaunched(level)).

% findall, maplist vs. repeat,fail,true
check_web_control :-
    retract(web_control(P)),
    ( catch(call(P), _Ex, fail)
     -> plog(web_control(P, called))
     ;	plog(touching(P)),
	atomic_list_concat(['./web/',P],File),  % Create signal file
	open(File,write,S),
	nl(S),
	close(File),
	plog(created(File))
    ),
    fail.
check_web_control.
% findall, maplist vs. repeat,fail,true
% findall(P,web_control(P),Ps),
% maplist(ex_web_control, Ps)
%
% ex_web_control(P) :-
%       catch(call(P),_,fail).
% ex_web_control(P) :-
%	atomic_list_concat(['./web/',P],File),  % Create signal file
%	open(File,write,S),
%	close(File).
% 

% From Web Form submission:  Attr=Value =>  <name>_<var>=<value>
% Results in sending <var><value> to object with <name>
changeRequest(List) :-  maplist(new_value,List).

evostatrestart :-
    plog('evostat getting restart!'),
    closeAll, % Bluetooth sockets
    plog('sockets closed'),
    stop_http,
    plog('http stopped'),
    sleep(1),
    read_link('/proc/self/exe',_,EvoStat),
    plog(oldpid(EvoStat)),
    catch(exec(EvoStat),Ex,(plog(execEx(Ex)),true)).  % The rest is silence

closeAll :-
    ( component(Name,_T,Obj),
      send(Obj,closesocket),
      plog(closing(socket(Name))),
      fail
     ; plog(finished(close(sockets)))
    ).

new_value(submit='Submit') :- !.
new_value(motd=Message)    :- !,
			      retractall(motd(_)),
			      assert(motd(Message)).
new_value(Attr=Value) :-
  atomic_list_concat([Name,Var],'_',Attr),
  ensure_value(Value, EValue), % atom -> int if possible
  component(Name, _, Obj),
  get(Obj, Var, OldValue),
  ( OldValue == EValue
   -> plog(unchanged(Name,Var))
   ;  send(Obj, Var, EValue),
      assert(changed(Obj,Var,EValue))
  ).

bars_ml(Bars, ML) :-
    nonvar(Bars),
    !,
    ML is 20 + Bars*20.
bars_ml(Bars, ML) :-
    ML > 20,
    Bars = integer((ML-20)/20).

propagate(end_of_data).
propagate(level(Name,Bars,Elapsed)) :-
    bars_ml(Bars,ML),
    component(Name,_,Obj),
    get(Obj,tl,TargetLevel),
    (Elapsed < 0
     -> plog(newlevel(Name,TargetLevel,ML:faked))
     ;  plog(newlevel(Name,TargetLevel,ML))
    ),
    send(Obj,l,ML),
    timeAtom(HumanTime),
    plog(volume(Name,ML,Elapsed,HumanTime)).

backgroundImage(ImageFile) :-
    config_name(Name,_),
    concat_atom(['./images/',Name,'.png'],ImageFile).

backgroundSettings(ImageFile) :-
    config_name(Name,_),
    concat_atom(['./images/',Name,'_Settings.png'],ImageFile).

log_file(F, Name, Header) :-
    ( exists_file(Name)
     ->	open(Name,append, F)
     ;	open(Name, write, F),
	maplist(write(F),Header)
    ).

header(Types, DataSet, Arity, Header) :-
    findall([':- ',Type,' ',Functor,'/',Arity,'.\n'],
	    ( member(_:Terms,DataSet),
	      member(_:Functor,Terms),
	      member(Type,Types) ),    DDs),
    flatten(['% Parameter( Component, Time, Value )\n',
	     ':- discontiguous timestamp/1.\n'|DDs],Header).
    
datalog :-
    get_time(NowFloat),
    format_time(atom(TS),'%T',NowFloat),
    Now is integer(NowFloat),
    DataSet = [ cellstat:[t:temperature,l:level,v1:valve,b:turbidity],
		lagoon:  [t:temperature,l:level,v1:valve,b:turbidity],
		sampler:  [v0:hostValve,v1:lagoonValve]],
    header([dynamic, multifile,discontiguous],DataSet,3,Header),
    log_file(F, 'web/datalog.txt', Header),
    ( write_term(F,timestamp(TS,Now),[fullstop(true),nl(true)]),
      dataset(Now, DataSet, Term), % Generator
      write_term(F,Term,[fullstop(true),nl(true)]),
      fail
     ; close(F)
    ).


ostreams(Names) :-
    setof(Name, S^stream_property(S, file_name(Name)), Names).
    

dataset(Time, Dataset, Term) :-
    member(Type:Data,Dataset),
    component(Name,Type,Obj),
    online(Obj),
    member(Var:Functor,Data),
    get(Obj,Var,Value),
    Term =.. [ Functor, Name, Time, Value ].

safe(Goal, Boolean) :-  % safe(+,Boolean specifies success or failure of exception)
    catch(Goal, Ex,
	  ( plog('EXCEPTION'(Goal,Ex)),
	    get_prolog_backtrace(30,BT),
	    print_prolog_backtrace(user_error,BT,[subgoal_positions(true)]),
	    Boolean,
	    default_binding(Goal) )
	 ).

safe_quiet(Goal, Boolean) :-
    catch(Goal, _, (default_binding(Goal)->Boolean;Boolean)).

:- dynamic user_default_binding/1.
:- multifile user_default_binding/1.

%! default_binding(?) is det.

default_binding(Goal) :- user_default_binding(Goal),!.
default_binding(Goal) :- plog(no_default_binding(Goal)), fail.

%! set_default_binding(+) is det.
set_default_binding(Goal) :-
    functor(Goal,F,A),
    functor(Template,F,A),
    retractall(user_default_binding(Template)),
    assert(user_default_binding(Goal)).

%! cache_call(+) is semidet.
cache_call(Goal) :-
    call(Goal),
    set_default_binding(Goal),
    !.
    

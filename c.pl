:- use_module(library(time)).
:- use_module(library(pce)).
:- use_module(library(process)).
:- use_module(library(charsio)).
:- use_module(library(helpidx)).
:- use_module(library(lists)).
:- use_module(library(ctypes)).
%:- pce_autoload(finder, library(find_file)).
%:- pce_global(@finder, new(finder)).
:- dynamic component/3.
    
temp_file('mypic1.jpg').
temp_file('mypic2.jpg').
temp_file('dbg.txt').
temp_file(File)       :- logfile(File).

cleanup :-
    temp_file(F),
    exists_file(F),
    delete_file(F),
    fail.
cleanup.

stop_updates :-
       send(@ut, stop),
       send(@action?members, for_all,
	    if(@arg1?value==stop,message(@arg1, active, @off))),
       send(@action?members, for_all,
	    if(@arg1?value==start,message(@arg1, active, @on))).

start_updates :-
       send(@ut, start),
       send(@action?members, for_all,
	    if(@arg1?value==start,message(@arg1, active, @off))),
       send(@action?members, for_all,
	    if(@arg1?value==stop, message(@arg1, active, @on))).

% All messages to logfile (otherwise, message window) Linux only
:- dynamic logfile/1.
% logfile(logfile).

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
    config(List),
    memberchk(mac(Num),List),
    concat_atom(['/dev/video',Num],Device).

% No camera reset on Windows (yet)
camera_reset :- current_prolog_flag(windows,true), !.

% No camera reset if no camera
camera_reset :-
    camera_device(Device),
    \+ access_file(Device,exist),
    writeln(no_camera),
    !.

camera_reset :-
    evostat_directory(Dir),
    process_create('/bin/rm',['-f','mypic1.jpg','mypic2.jpg'],[cwd(Dir)]),
    Cmd = '/usr/bin/uvcdynctrl',
    config_name(Config),    % Hostname or evostat argument
    concat_atom([Config,'.gpfl'], Settings),
    camera_device(Device),
    concat_atom(['--device=',Device],Option),
    Args = ['-L', Settings, Option],
    writeln(resettingCamera(Dir, Cmd, Args)),
    process_create(Cmd,Args,[cwd(Dir)]),
    writeln(resetCamera).

windows :- current_prolog_flag(windows,true).

evostat_directory('C:\\cygwin\\home\\peterr\\src\\EvoStat\\') :-
  gethostname('ncmls8066.ncmls.org'),!.
evostat_directory('C:\\cygwin\\home\\peter\\src\\EvoStat\\')  :- windows, !.
evostat_directory('/home/peter/src/EvoStat/').
	
python('C:\\Python27\\python.exe')         :- gethostname(elapse),!.
python('C:\\cygwin\\Python27\\python.exe') :- windows, !.
python('/usr/bin/python').

:- [gbutton].
:- [dialin].

:- dynamic target_value/2, current_value/4, current_value/2, screen/5.
:- dynamic component/2, levelStream/1, air/0, mix/0, toggle_auto/0.

%
% System Configuration
% Dialog Layout
% Communications Information
%
% pathe.config
%
%
% After device discovery process, the evostat specification
% will be written to evostat.config with verified BT addresses.
% Shutting down should preserve verified bluetooth addresses
% as well as target values for temperature, turbidity, and flow.
%
% Color code for labeling text (name or parameter values):
%   Red before connections are established
%   Blue when parameter values are low
%   Orange when parameters are high
%   Green when paramaters are in the target range
% To create your own configuration edit a copy of template.pl named
%  <hostname>.pl (or <evostatname>.pl if more than one on the same computer).
% The 'evostat' program reads this file and creates <hostname>.settings
% for the the Python programs (ipcam.py, level.py, fluor.py, pH.py)
%
% You can edit the .settings file to test/debug the Python programs
% but it will be overwritten whenever you run: 'evostat'
%
:- use_module(library(lists), [ flatten/2 ] ).

:- dynamic config/1.       % Loaded configuration
:- dynamic supress/1.      % Terms to exclude from dictionary (see grammar below)
:- dynamic tabs/1.
tabs(1).

:- dynamic debug/0.
debug.                % Will be retracted by save_evostat (building binary)


check_file(Root) :-   % consult(foo) will work for files named foo or foo.pl
	( exists_file(Root)
        -> true
        ; concat_atom([Root,'.pl'],File),
	  exists_file(File)
        ).

% config_name(-Root)

config_name(Root) :-
	current_prolog_flag(argv,[_Exe|Args]),  % Command-line argument
	member(Root,Args),
	check_file(Root),
	!.

config_name(Root) :-
	gethostname(Name),    % <HOSTNAME>.pl configuration file
	atom_chars(Name,Cs),
	( append( RCs,['.'|_],Cs ) % Could be full domain name ('x.y.com')
        -> atom_chars(Root,RCs)
        ;  Root = Name
        ),
	check_file(Root).

% Convert Prolog term to Python dictionary

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
                 
% Addition 'logfile' messages: pathe_report/1 will be called on exit
% We call append/1 because redirected logfile output has been closed.

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
    catch( get(@gui, graphicals, Chain),
	   ( chain_list(Chain, CList), freeall(CList) ),
	    writeln(firsttime)).
	   
freeall([]).
freeall([H|T]) :- writeln(free(H)), free(H), freeall(T).

check_error(camera(IP))    :- writeln(error(camera(IP))),!,fail.
check_error(othererror(D)) :- writeln(error(othererror(D))),!,fail.
check_error(_).               % Everything else is not an error

%
% Find the lagoon(object) for a given position, names can be
% complex but must start with 'lagoon' and end with a single digit
%
lagoon_number(Object, N) :-
    component(Lagoon, lagoon, Object),
    atom_codes(Lagoon, Codes),
    Digit is N + 0'0,
    append(_,[Digit],Codes).

lagoons(Cmd) :-
    component(_, lagoon, Object),
    send(Object,Cmd),
    fail.
lagoons(_).

send_info(flux(F),Stream) :- !, newFlux(flux(F),Stream).
send_info(levels(Level),_) :-
    lagoon_number(Object, 3),
    send(Object, setLevel, Level).
send_info(_,_).

get_new_levels :-
    lagoons(quiet),
    python(Python),
    evostat_directory(Dir),
    concat_atom([Dir,'ipcam.py'],IPCAM),
    CmdLine = [IPCAM],
    ( retract(levelStream(Previous)) ->
	catch( read(Previous, Info),
	       Ex,
	       (writeln(caught(Ex,CmdLine)),sleep(3),fail)),
        check_error(Info),
	send_info(Info, Previous),
	close(Previous)
     ; true
    ),
    evostat_directory(Dir),
    writeln(process_create(Python,CmdLine,[stdout(pipe(Out)),cwd(Dir)])),
    process_create(Python,CmdLine,[stdout(pipe(Out)),cwd(Dir)]),
    assert(levelStream(Out)),
    !.

get_new_levels :- 
    writeln(failed(levelupdate)).


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
          writeln(evostat_object(W)),
	  screen(WW,WH,DW,DH,Location),
	  EWidth is WW*DW/100,
	  EHeight is WH*DH/100,
          send(W, size, size(EWidth, EHeight)),
	  writeln(evostat(width(EWidth),height(EHeight))),

% MENU BAR
	  send(W,  append, new(MB, menu_bar)),
	  send(MB, label_font(huge)),
	  writeln(menu_bar(MB)),

	  new(Msg1, message(W, update10)),  % Create Timer Object
	  free(@ut),
	  send(W, attribute, attribute(timer, new(@ut, timer(20.0, Msg1)))),

	
	  send(MB, append, new(File, popup(file))),
          free(@action),
	  send_list(File, append,
				  [
				    menu_item(ok,
					      message(W, return, ok)),
				    menu_item(quit,
					      message(W, quit))
				  ]),
	  send(MB, append, new(@action, popup(action))),
	  send_list(@action, append,
				  [ menu_item('Drain Lagoons',
					      message(W, drain, lagoons)
					     ),
				    menu_item('Drain Cellstat',
					      message(W, drain, cellstat)
					     ),
				    menu_item(stop,
					      message(W, stopped)),
				    menu_item(start,
					      message(W, started))
				  ]),
	  send(MB, append, new(Help, popup(help))),
	  about_atom(About),
	  send_list(Help, append,
				  [ menu_item(about,
					      message(@display, inform, About)),
				    menu_item(debug,
					      message(@prolog, manpce))
				  ]),
         call(Label,Components),
         findall(_,(component(_,_,Obj),free(Obj)),_), % Clear out previous
	 maplist(create(@gui), Components),

         send(W,started),
         send_super(W, open, Location).

drain(_W, What) :->  writeln(draining(What)).

stopped(_W) :->
       send(@ut, stop),
       send(@action?members, for_all,
	    if(@arg1?value==stop,message(@arg1, active, @off))),
       send(@action?members, for_all,
	    if(@arg1?value==start,message(@arg1, active, @on))).

started(_W) :->
       send(@ut, start),
       send(@action?members, for_all,
	    if(@arg1?value==start,message(@arg1, active, @off))),
       send(@action?members, for_all,
	    if(@arg1?value==stop, message(@arg1, active, @on))).


cellstat(_W) :-> "User pressed the CellStat button"::
%        ( air ->
%	     retract(air), Cmd = 'o-'
%	 ;   assert(air), Cmd = 'o2'
%	),
        ( mix ->
	     retract(mix), Cmd = 'm0'
	 ;   assert(mix), Cmd = 'm1'
	),
        component(_,cellstat,CellStat),
        send(CellStat,converse,Cmd).

l1(_W) :-> "User pressed the L1 button"::
  current_prolog_flag(argv,[_,X|_]),
  send(@l1, label, X).

lagoon1(_W) :->
       "User selected Lagoon 1"::
       component(lagoon1,lagoon,L), writeln(calibrate(lagoon1)),
        ( toggle_auto ->
	     retract(toggle_auto), Cmd = 'a0'
	 ;   assert(toggle_auto), Cmd = 'a1'
	),
        send(L,converse,Cmd).

lagoon2(_W) :->
       "User selected Lagoon 2"::
       component(lagoon2,lagoon,L), writeln(calibrate(lagoon2)), send(L,calibrate).

lagoon3(_W) :->
       "User selected Lagoon 3"::
       component(lagoon3,lagoon,L),
        ( toggle_auto ->
	     retract(toggle_auto), Cmd = 'o2'
	 ;   assert(toggle_auto), Cmd = 'o-'
	),
        send(L,converse,Cmd).

lagoon3d(_W) :->
       "User selected Lagoon 3-Darwin"::
       component(lagoon3d,lagoon,L),
        ( toggle_auto ->
	     retract(toggle_auto), Cmd = 'o2'
	 ;   assert(toggle_auto), Cmd = 'o-'
	),
        send(L,converse,Cmd).

lagoon4(_W) :->
       "User selected Lagoon 4"::
       component(lagoon4,lagoon,L), writeln(calibrate(lagoon4)), send(L,calibrate).

newvalue(Name,Parent) :-
        get(getValue('New Target Value'), prompt, String),
	catch(atom_number(String,Value),error(type_error(_,_),_),fail),
        retract(target_value(Name,_)),
        assert(target_value(Name, Value)),
        send(Parent,update).


quit(W) :->
        "User pressed the Quit button"::
%	send(@ut, stop),     % Shut down the label update timer
	retractall(current_value(_,_,_,_)),
	retractall(current_value(_,_)),
	retractall(target_value(_,_)),
        send(W, return(quit)).

load(W, File:[file]) :->
        "User pressed the Load button"::
%	send(@ut, stop),     % Shut down the label update timerg
	retractall(current_value(_,_,_,_)),
	retractall(current_value(_,_)),
	retractall(target_value(_,_)),
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


% MUCH SIMPLER

update10(W) :->
    stop_updates,
    get_new_levels,
    writeln('update10 after get_new_levels'),
    send(W?graphicals, for_all,
	 if(message(@arg1,instance_of,ebutton),message(@arg1,update))),
    send(W?graphicals, for_all,
	 if(message(@arg1,instance_of,snapshot),message(@arg1,update))),
    writeln('update10 COMPLETED'),
    start_updates.

%    get(W, graphicals, Chain),
%    chain_list(Chain, CList),
%    writeln('update [ '),
%    member(Object, CList),
%    component(_,_,Object),            % If one has been created
%    ( send(Object, update) -> write(Object) ; write(failed(Object)) ),
%    write(' '),
%    flush_output,
%    fail.

%update10(_W) :-> writeln(' ]').
%    writeln(finishedupdate10(W)).
    
:- pce_end_class.

righton :-
    retract(current_value(t2,_,_,_)),
    assert(current_value(t2,l2,temperature,36)).

raise :-
    current_value(t2,_,_,T),
    retract(current_value(t2,_,_,T)),
    NewT is T + 2,
    assert(current_value(t2,l2,temperature,NewT)).

lower :-
    current_value(t2,_,_,T),
    retract(current_value(t2,_,_,T)),
    NewT is T - 2,
    assert(current_value(t2,l2,temperature,NewT)).

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
bluetalk(   S, Cmd,     Reply    ) :- bt_converse(S ,Cmd, Reply).
bluetalk(   _,   _, 'send_failed.'    ).


c :- main([]).

c(Name) :-
    free(@gui),
    new(@gui, evostat(Name)),
    send(@gui?frame, icon, bitmap('./evo.xpm')),
    get(@gui, prompt, Reply),
    (Reply = quit ->
         send(@gui, destroy)
     ;   true
    ).

% Making a Prolog executable (saved-state)
% :- [c],save_evostat.
% main(Argv) :-  start application here, using passed arguments from Argv
%

%
% Configuration <name> is either a command-line argument or <hostname>
% The corresponding Prolog file ( <name>.pl ) must exist.
% Load configuration and generate Python .settings (dictionary) file. 


main :-      pce_main_loop(main).

main(_Argv) :-
        assert(file_search_path('C:\\cygwin\\home\\peter\\src\\EvoStat')),
%        use_module(library(pce)),
    
        ( logfile(_), current_prolog_flag(windows, true)
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
	( current_prolog_flag(windows, true)
         -> load_foreign_library(foreign(plblue))
	  ; load_foreign_library(plblue)
        ),
%        load_foreign_library('C:\\cygwin\\home\\peter\\src\\EvoStat\\plblue'),

	config_name(Root),  %  Find configuration name (hostname or cmd arg)
	consult(Root),      % Consult it
	config(List),       % Get the configuration data
        member(screen(WF,HF,Loc),List),
        get(@display?size,width,Width),
        get(@display?size,height,Height),
        assert(screen(Width,Height,WF,HF,Loc)),
        camera_reset,
	member(layout(Components),List),
	Layout =.. [Root,Components],
	assert(Layout),
%	writeln(Layout),

	assert(supress(layout(_))),           % Leave this out of the Python "settings" dictionary
	assert(supress(screen(_,_,_))),       %     '' 
	pl2PythonDict(List, PyString),        % Convert to Python Dictionary
	concat_atom([Root,'.settings'],File), % Write it out
	tell(File),
	write(PyString),
	told,
	c(Root),
        !.

os_emulator('C:\\cygwin\\swipl\\bin\\swipl-win.exe') :-
    current_prolog_flag(windows, true),!.
    
os_emulator('/home/peter/bin/swipl') :- % Haldane at SplatSpace
     gethostname(haldane),
     !,
     pce_autoload_all,
     pce_autoload_all.

os_emulator('/usr/bin/swipl') :- % Linux
     pce_autoload_all,
     pce_autoload_all.

% 'C:\\cygwin\\swipl\\bin\\swipl-win.exe'
%  '/cygdrive/c/cygwin/swipl/bin/swipl-win.exe'
%  '/usr/bin/swipl-win'
%  swi('bin/xpce-stub.exe'
%  swi('bin/swipl-win.exe'

save_evostat :-
    os_emulator(Emulator),
    retractall(debug),
    Options = [stand_alone(true), goal(main)],
    qsave_program(evostat, [emulator(Emulator)|Options]).






:- use_module(library(time)).
:- use_module(library(pce)).
:- use_module(library(process)).
:- use_module(library(charsio)).
:- use_module(library(helpidx)).
:- use_module(library(lists)).
:- use_module(library(ctypes)).
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

camera_reset(List) :-
    level_cmd_dir(_,Dir),
    Cmd = '/usr/bin/uvcdynctrl',
    config_name(Host),
    concat_atom([Host,'.gpfl'], Settings),
    memberchk(mac(Num),List),
    concat_atom(['--device=/dev/video',Num],Device),
    Args = ['-L', Settings, Device],
    writeln(resettingCamera(Dir, Cmd, Args)),
    process_create(Cmd,Args,[cwd(Dir)]),
    writeln(resetCamera(Cmd,Args)).

level_cmd_dir(['C:\\Python27\\python.exe','ipcam.py'],
	       'C:\\cygwin\\home\\peter\\src\\EvoStat') :-
    gethostname(elapse),
    current_prolog_flag(windows,true), !.

level_cmd_dir(['C:\\cygwin\\Python27\\python.exe','ipcam.py'],
	       'C:\\cygwin\\home\\peterr\\src\\EvoStat') :-
  current_prolog_flag(windows,true), !.

level_cmd_dir(['/usr/bin/python','/home/peter/src/EvoStat/ipcam.py'],
	      '/home/peter/src/EvoStat').

:- [gbutton].

:- dynamic target_value/2, current_value/4, current_value/2, screen/4.
:- dynamic component/2, levelStream/1, air/0, toggle_auto/0.

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


% All messages to logfile (otherwise, message window) Linux only
:- dynamic logfile/1.
%logfile(logfile).

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

resize(Thing) :-
    screen(_,_,Height,_),
    BH is Height/16,
    BW is 2*BH,
    send(Thing, size(size(BW,BH))).

create_label(value(Device,Parameter,Target),Name) :-
    assert(target_value(Name, Target)),
    assert(current_value(Name, Device, Parameter, 0)).

create_label(text(Label),Name) :-
    assert(current_value(Name, evostat, label, Label)).

row_item(button(Name,What),@Name) :-
    free(@Name),
    new(@Name,ebutton(Name)),
    send(@Name, accelerator, @nil),
    resize(@Name),
    send(@Name, colour(red)),
    new(M, move_gesture),
    new(P, popup_gesture(new(Pop, popup))),
    send_list(Pop, append,
              [ menu_item(reconnect, message(@Name,colour,colour(black)))
                , menu_item(disconnect,message(@Name,colour,colour(white)))
                , menu_item(dark, message(@Name,colour,colour(black)))
                , menu_item(light,message(@Name,colour,colour(white))) 
	      ]),
    new(G, handler_group(M, P)),
    send(@Name, recogniser, G),
    create_label(What, Name).

%    new(H1, message(File, displayed, @on)),

%    new(HIn, message(@Name, colour, colour(brown))),
%    new(Hout, message(@Name, colour, colour(red))),
%    send(@Name, recogniser,handler(area_enter, HIn)),
%    send(@Name, recogniser,handler(area_exit, Hout)),


row_item(cellstat(Name, text(Evo), size(Width,Height)),@Name) :-
    free(@Name),
    new(@Name,button(Name)),
    send(@Name, colour(blue)),
    send(@Name, size, size(Width, Height)),
    create_label(text(Evo),Name).

row_item(spacer(Color,Height), @Spacer) :-
    atom(Color),
    screen(_,Width,_H,_Location),
    NWid is Width - 30,
    new(@Spacer, box(NWid,Height)),
    send(@Spacer, colour, Color),
    send(@Spacer, fill_pattern, colour(Color)).

%row_item(label(Name,_What),@Name) :-
%    free(@Name),
%    new(@Name,button(Name)),
%    send(@Name, colour(red)).

row_item(image(Name,File),@Name) :-
    free(@Name),
    new(@Name, label(Name)),
    new(I, image(File)),
%    screen(_,Width,_,_),
%    NewWidth is Width - Width/10,
%    NewHeight is integer(Width*0.675),
%    get(I, size, Size),
%    send(Size, width(NewWidth)),
%    send(Size, height(NewHeight)),
    send(@Name, selection, I).

freeall :-
    catch( get(@gui, graphicals, Chain),
	   ( chain_list(Chain, CList), freeall(CList) ),
	    writeln(firsttime)).
	   
freeall([]).
freeall([H|T]) :- writeln(free(H)), free(H), freeall(T).

check_error(camera(IP))    :- writeln(error(camera(IP))),!,fail.
check_error(othererror(D)) :- writeln(error(othererror(D))),!,fail.
check_error(_).               % Everything else is not an error

send_levels([],_).
send_levels([Level|Levels], N) :-
     concat_atom(['lagoon',N],Lagoon),
     send(@Lagoon, setLevel, Level),
     NN is N-1,
     send_levels(Levels,NN).

get_new_levels :-
    ( retract(levelStream(Previous)) ->
	catch(read(Previous, Levels),Ex,(writeln(caught(Ex,Cmd)),fail)),
        check_error(Levels),
        ( Levels =..[levels|LVals] ->
	   config(List),
           member(numLagoons(NL),List),
           send_levels(LVals, NL)
        ;
	   newFlux(Levels,Previous)
        ),
	close(Previous)
    ; true
    ),
    level_cmd_dir([Cmd|Args],Cwd),
    process_create(Cmd,Args,[stdout(pipe(Out)),cwd(Cwd)]),
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
          screen(Label,WinWidth,WinHeight,Location),
          send(W, size, size(WinWidth, WinHeight)),

% MENU BAR
	  send(W, append, new(MB, menu_bar)),
	  send(MB, append, new(File, popup(file))),
	  send(MB, append, new(Help, popup(help))),
	
		send_list(File, append,
				  [ menu_item(load,
					      message(W, load, @finder?file)),
				    menu_item(ok,
					      message(W, return, ok)),
				    menu_item(quit,
					      message(W, quit))
				  ]),
		about_atom(About),
		send_list(Help, append,
				  [ menu_item(about,
					      message(@display, inform, About)),
				    menu_item(debug,
					      message(@prolog, manpce))
				  ]),
         call(Label,Components),
         findall(_,(component(_,Obj),free(Obj)),_), % Clear out previous
	 maplist(create(@gui), Components),
	 new(Msg1, message(W, update10)),
	 free(@ut),
	 send(W, attribute, attribute(timer, new(@ut, timer(20.0, Msg1)))),
	 send(@ut, start),
         send_super(W, open, Location).

cellstat(_W) :-> "User pressed the CellStat button"::
        ( air ->
	     retract(air), Cmd = 'o-'
	 ;   assert(air), Cmd = 'o2'
	),
        component(cellstat,CellStat),
        send(CellStat,converse,Cmd).

l1(_W) :-> "User pressed the L1 button"::
  current_prolog_flag(argv,[_,X|_]),
  send(@l1, label, X).

lagoon1(_W) :->
       "User selected Lagoon 1"::
       component(lagoon1,L), writeln(calibrate(lagoon1)),
        ( toggle_auto ->
	     retract(toggle_auto), Cmd = 'a0'
	 ;   assert(toggle_auto), Cmd = 'a1'
	),
        send(L,converse,Cmd).

lagoon2(_W) :->
       "User selected Lagoon 2"::
       component(lagoon2,L), writeln(calibrate(lagoon2)), send(L,calibrate).
lagoon3(_W) :->
       "User selected Lagoon 3"::
       component(lagoon3,L), writeln(calibrate(lagoon3)), send(L,calibrate).
lagoon4(_W) :->
       "User selected Lagoon 4"::
       component(lagoon4,L), writeln(calibrate(lagoon4)), send(L,calibrate).

tb(W)   :-> newvalue(tb,W).
tc(W)   :-> newvalue(tc,W).

t1(W)   :-> newvalue(t1,W).
t2(W)   :-> newvalue(t2,W).
t3(W)   :-> newvalue(t3,W).
t4(W)   :-> newvalue(t4,W).

lux1(W) :-> newvalue(lux1,W).
lux2(W) :-> newvalue(lux2,W).
lux3(W) :-> newvalue(lux3,W).
lux4(W) :-> newvalue(lux4,W).

flow1(W) :-> newvalue(flow1,W).
flow2(W) :-> newvalue(flow2,W).
flow3(W) :-> newvalue(flow3,W).
flow4(W) :-> newvalue(flow4,W).


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
%	send(@ut, stop),     % Shut down the label update timer
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

update10(W) :->
    get_new_levels,
    get(W, graphicals, Chain),
    chain_list(Chain, CList),
    write('update [ '),
    member(Object, CList),
    component(_Name,Object),        % If one has been created
    ( send(Object, update) -> write(Object) ; write(failed(Object)) ),
    write(' '),
    flush_output,
    fail.

update10(_W) :-> writeln(' ]').
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


:- pce_begin_class(getValue, dialog, "Change a Value").

initialise(W, Label:[name]) :->
        "Initialise the window and fill it"::
        send_super(W, initialise(Label)),
        send(W, append(text_item(name))),
        send(W, append(button(ok))),
        send(W, append(button(cancel))),
        send(W, default_button(ok)).
        
ok(W) :->
        "User pressed the OK button"::
        get(W, member(name), NameItem),
        get(NameItem, selection, Typed),
        send(W, return, Typed).

cancel(W) :->
        "User pressed the Cancel button"::
        send(W, return(@nil)).

prompt(W, Value:name) :<-
        "Open it, destroy it and return the result"::
        get(W, confirm, Value),
        free(W).

:- pce_end_class.

% Initializers are extra arguments to the constructor
% Data is a list of messages to continue initializing the object

create(Dialog, Component) :-
	Component =.. [Type, Name, Position, Data],
	free(@Name),
	Class =.. [Type,Name],
	new(@Name, Class),
	maplist(send(@Name), Data), % Process all before appending
	send(Dialog, append(@Name, Position)),
%	writeln(component(Name,@Name)),
        assert(component(Name,@Name)).

about_atom(About) :-
        open('evostat.about', read, Handle),
	read_pending_input(Handle,FileContent,[]),
	atom_chars(About,FileContent).

% gethostname returns the full domain name on some systems
hostname_root(H) :-
     gethostname(Name),
     atom_chars(Name,Cs),
     ( append(RCs,['.'|_],Cs) -> atom_chars(H,RCs) ; H = Name ).

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
        use_module(library(pce)),
        ( logfile(_), current_prolog_flag(windows, true)
         -> retract(logfile(_))
         ; true
        ),
        level_cmd_dir(_,HomeDir),
        cd(HomeDir),              % savestate can be run from anywhere
	(logfile(File)
	 -> tell(File),
	    telling(S),
	    set_stream(S,buffer(line)),
	    set_stream(user_error,buffer(line)),
	    set_stream(S,alias(user_error))
	 ; true
        ),
        current_prolog_flag(pid,PID),
        (PID < 900 -> sleep(30) ; true),  % Delay if OS just started (low PID)
        set_prolog_flag(save_history,false),
	at_halt(pathe_report(verbose)),
%        load_foreign_library(foreign(plblue)),
        load_foreign_library(plblue),

	config_name(Root),          %  Find out configuration name
	consult(Root),              % Consult it
	config(List),                         % Get the configuration data
%	writeln(configuration(List)),

	member(screen(W,H,Pos), List),
	assert(screen(Root,W,H,Pos)),
        camera_reset(List),

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

os_emulator(E) :-    
    current_prolog_flag(windows, true),
    !,
    ( logfile(_)
     -> E = swi('bin/xpce-stub.exe') % All msgs to logfile
     ;  E = swi('bin/swipl-win.exe') % Use window for msgs
    ).

os_emulator('/usr/bin/swipl').


save_evostat :-
    os_emulator(Emulator),
    retract(debug),
    pce_autoload_all,
    pce_autoload_all,
    Options = [stand_alone(true), goal(main)],
    qsave_program(evostat, [emulator(Emulator)|Options]).






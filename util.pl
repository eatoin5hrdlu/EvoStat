%%%%%%%%%%% OS IDENTIFICATION
windows :- current_prolog_flag(windows,true).
unix    :- current_prolog_flag(unix,true).
linux   :- unix.

%%%%%%%%%%% RUNNING EXTERNAL PROGRAMS (python, etc.)
:- use_module(library(process)).
:- use_module(library(charsio)).
:- use_module(library(helpidx)).
:- use_module(library(lists)).
:- use_module(library(ctypes)).

message(F,L) :- format(user_error, F, L).

:- dynamic component/3,
           leak/1,
	   temperature/3,
	   last_level/2,
	   cycle/1.

:- dynamic evoDir/1.               % <evostat>.pl configuration file
:- multifile evoDir/1.

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

 
cleanup :- findall(F,(temp_file(F),exists_file(F),delete_file(F)),_).

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
% All messages to logfile (otherwise, stderr)
% Redirection breaks in Windows, so no logfile

plog(Term) :- write(user_error,Term),nl(user_error).

logging :- windows, !, retractall(logfile(_)).
logging :- ( logfile(File)
            -> tell(File),
	       telling(S),
	       set_stream(S,buffer(line)),
	       set_stream(user_error,buffer(line)),
	       set_stream(S,alias(user_error))
	    ; plog(no_log_file)
	   ).

:- use_module(library(lists), [ flatten/2 ] ).

:- dynamic config/1.       % Loaded configuration
:- dynamic file_modtime/2. % Modification time of loaded file
:- dynamic param/4.        % param(Name, Type, Attr, Value)

param(P) :- config(List), memberchk(P,List).

check_file(Root,File) :-  % consult(foo) works files 'foo' or 'foo.pl'
	( exists_file(Root)
        -> true
        ; concat_atom([Root,'.pl'],File),
	  exists_file(File)
        ).

freeall :-
    get(@gui, graphicals, Chain),
    chain_list(Chain, CList),
    maplist(free,CList).

send_to(Recv,List) :-
    if(message(Recv,instance_of,chain),
       ( MSG =.. [message,@arg1|List], send(Recv,for_all,MSG) ),
       ( MSG =.. List, send(Recv,MSG))  ).


% E.g. control_timer(texting,  {start,stop} ).
%      control_timer(    pid,  {start,stop} ).
%      control_timer( update,  {start,stop} ).
condition(start, on, off).
condition(stop,  off, on).

control_timer(Thing,StartStop) :-
    concat_atom([Thing,timer], TimerName),
    concat_atom(['no ', Thing], NoThing),
    send(Thing, StartStop),
    condition(StartStop, A, B), % Ghost-out other menu item
    send(@action?members, for_all,
	 if(@arg1?value==Thing,message(@arg1, active, @A))),
    send(@action?members, for_all,
	 if(@arg1?value==NoThing,message(@arg1, active, @B))),
    send(@TimerName,StartStop).
    
% gethostname returns the full domain name on some systems
hostname_root(H) :-
     gethostname(Name),
     atom_chars(Name,Cs),
     ( append(RCs,['.'|_],Cs) -> atom_chars(H,RCs) ; H = Name ).

load_newest(File) :-
    file_modtime(File, Time),                  % Last loaded Time
    source_file_property(File,modified(Time)), % Matches!
    !.

load_newest(File) :-
    consult(File),
    plog(consulted(File)),
    source_file_property(File,modified(Time)),
    retractall(file_modtime(File,_)),
    assert(file_modtime(File, Time)).

count(ErrorType, Who) :-
    Prev =.. [ErrorType, _When, HowMany],
    ( retract(err(Who,Prev)) -> NErrors is HowMany+1 ;  NErrors=1 ),
    timestring(Now),
    Result =.. [ErrorType, Now, NErrors],
    assert(err(Who,Result)).

:- dynamic standalone.
save_evostat :-
    retractall(debug),
    assert(standalone),
    current_prolog_flag(executable,E),
    Options = [stand_alone(true), goal(main)],
    qsave_program(evostat, [emulator(E)|Options]).

% Given a Cbhdir path of depth N,
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

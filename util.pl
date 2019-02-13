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

:- dynamic config/1,
           file_modtime/2, % Modification time of loaded file
           logfile/1,      %
           component/3,
           leak/1,
	   cycle/1,
           bt_device/2,    %
           webok/0.

cleanup :- findall(F,(temp_file(F),exists_file(F),delete_file(F)),_).

touch(File) :- open(File,append,Stream),close(Stream).

offline :-  % Does the word OFFLINE appear in the Message of the Day?
    motd(MOTD),
    string_lower(MOTD,Motd),
    sub_string(Motd,_,_,_,offline),
    plog(offline).

online :- \+ offline, plog(online).

%%%%% TIME STUFF
timeAtom(Atom) :-
     get_time(TimeStamp),
     format_time(atom(Atom), '%A, %d %b %Y %T', TimeStamp, posix).
    
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

% supress some debugging we want to turn on later easily
plog(updating(X)) :-
    \+ member(X,[evostat,host0,lagoon2]),
    !.
plog(updated(X)) :-
    \+ member(X,[evostat,host0,lagoon2]),
    !.
plog(bluetalk_returned(_,Cmd,_)) :-
    member(Cmd,[b,t,tt,w,m0,m1,o2,'o-']),
    !.
plog(Term) :-
    write(user_error,Term),nl(user_error).

:- dynamic flowStream/1.
flog(Term) :- ( flowStream(S)
	       -> write(S,Term),nl(S),flush_output(S)
	       ; open('web/flog.txt',write,S,[]),
		 assert(flowStream(S)),
		 flog(Term)
	      ).

:- dynamic dbStream/2.

dbstream(Type,Stream) :-
    ( dbStream(Type,Stream) -> true
    ; concat_atom(['web/',Type,'log.txt'],File),
      open(File,append,Stream,[]),
      assert(dbStream(Type,Stream)),
      dbtimestamp(Type)
    ).

dbtimestamp(Type) :-
      timestring(Now),
      Stamp =.. [Type, Now],
      dblog(Type,Stamp).
    
dblog(Term) :-
    functor(Term,Type,_),
    dblog(Type,Term).
    
dblog(Type,Term) :-
    dbstream(Type,S),
    write(S,Term),nl(S),flush_output(S).

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

send_to(Recv, List) :-
    if(message(Recv,instance_of,chain),
       ( MSG =.. [message,@arg1|List], send(Recv,for_all,MSG) ),
       ( MSG =.. List, send(Recv,MSG))  ).

send_to_type(Recv,Type,List) :-
    MSG =.. [message,@arg1|List],
    send(Recv, for_all, if(message(@arg1,instance_of,Type), MSG)).

% E.g. control_timer(texting,  {start,stop} ).
%      control_timer(    pid,  {start,stop} ).
%      control_timer( update,  {start,stop} ).

control_timer(Thing,StartStop) :-
    ghost_state(Thing, StartStop),
    concat_atom([Thing,timer], TimerName),
    send(@TimerName,StartStop).

condition(start, off, on).
condition(stop,  on, off).

ghost_state(Thing, Condition) :-
    concat_atom(['no ', Thing], NoThing),
    condition(Condition, A, B), % Ghost-out other menu item
    send(@action?members, for_all,
	 if(@arg1?value==Thing,message(@arg1, active, @A))),
    send(@action?members, for_all,
	 if(@arg1?value==NoThing,message(@arg1, active, @B))).
    
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

% Run python progam and consult its output
% Consult (e.g. reconsult) replaces predicates unless multifile/1 declared
% Retry (involves resetting camera) when image too dark (exit(5))
    
consult_python(Program, Options, ReTry) :-
    python(Python),
    evostat_directory(Dir),
    process_create(Python,[Program|Options],
		   [stdout(pipe(Out)), stderr(std), cwd(Dir),process(PID)]),
    process_wait(PID, Exit, [timeout(10)]),
    ( Exit = exit(0)
      -> load_files(Program,[stream(Out)])
      ;  true
    ),
    close(Out),
    ( Exit = exit(5), ReTry > 0
      -> Next is ReTry - 1,
         plog(retry(consult_python(Program,Options,Exit,ReTry))),
	 consult_python(Program, Options, Next)
      ;  plog(failed(consult_python(Program,Options,Exit,ReTry)))
    ).

count(ErrorType, Who) :-
    Prev =.. [ErrorType, _When, HowMany],
    ( retract(err(Who,Prev)) -> NErrors is HowMany+1 ;  NErrors=1 ),
    timestring(Now),
    Result =.. [ErrorType, Now, NErrors],
    assert(err(Who,Result)).

save_evostat :-
    current_prolog_flag(executable,E),
    Options = [stand_alone(true), goal(pce_main_loop(main))],
    qsave_program(evostat, [emulator(E)|Options]).

repeat(N) :- integer(N), % type check
             N>0,        % value check 
             repeatN(N).

repeatN(1) :- !,plog(repeatN_exhausted),fail.
repeatN(_).
repeatN(N) :- M is N-1, repeatN(M).

waitfor(N,Atom,_) :-
   repeat(N),
   ( call(Atom) -> ! ; sleep(0.2), fail ).

waitfor(_,Atom,Who) :-
    plog(failed(Who,waitfor,Atom)),
    fail.
    
semaphore(N,Atom,_) :- % wait for and grab it
   repeat(N),
   ( retract(Atom) -> true ; sleep(0.2), fail ),
   !.

semaphore(_,Atom,Who) :- % Report failure and reassert
    plog(failed(Who,semaphore,Atom)),
    assert(Atom),
    fail.

% Force atoms with only digits/decimal point to be numeric
ensure_value(Atom, Number) :-
        atom_codes(  Atom,   ACodes),
        ( ACodes = [0'.|Codes] -> true ; Codes=ACodes),
        number_codes(Number, Codes),
        !.
ensure_value(Atom, Atom).

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
timeatom(Hours,M,S,TimeAtom) :-
	format(atom(Minutes), '~|~`0t~w~2|', M),
	format(atom(Seconds), '~|~`0t~w~2|', S),
	concat_atom([Hours,':',Minutes,':',Seconds],TimeAtom).
	
window_percent(WFraction,HFraction,W,H) :-
	screen(DW,DH, WW, WH, _Loc),
	W is integer(DW*WW*WFraction/10000),
	H is integer(DH*WH*HFraction/11000).

% E.g. leak detection: compare_delta(Op,N,500,200),
compare_delta(Op, A, Base, Delta) :-
    Min is Base - Delta,
    Max is Base + Delta,
    compare_minmax(Op,A,Min,Max).

compare_minmax(>, Val,  _, Max) :- Val > Max, !.
compare_minmax(<, Val, Min,  _) :- Val < Min, !.
compare_minmax(=,   _,   _,  _). % In range

constrain(>, _,    _, Max, Max).
constrain(<, _,  Min,   _, Min).
constrain(=,Val,  _,    _, Val).

upperName(Out) :-
    hostname_root(Hostname),
    atom_codes(Hostname,[H|T]),
    to_upper(H,UH),
    atom_codes(Out,[UH|T]).

htmlspace(0,[]).
htmlspace(N,[&(nbsp)|T]) :- N > 0, NN is N-1, htmlspace(NN,T).

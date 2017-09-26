:- set_prolog_flag(double_quotes,codes).
:- use_module(library(pce)).
:- op(1200,xfx,':->'). % Define operators for PCE term-expansion
:- op(1200,xfx,':<-').
:- op(910, xfx,'::').
:- dynamic webValue/3.
:- dynamic offline/1.

% Generate XPCE/HTML interfaces for Arduino devices
% Arduino response to 'i' is iface(Class, Parent, Variables)
% E.g. iface(lagoon, ebutton,
%          [int(temperature,   t, ro, "Temperature"),
%           int(ttemperature, tt, rw, "Target Temperature")]).
%
% Parent class (ebutton) contains:
% rw variables: [socket, {temperature, turbidity, volume}Units ]
% and methods : [ connect/1, converse/2, parse_reply_arg1/3 ]
% Variable myname is created in the new class

:- dynamic changed/3.   % Assertion to tell system to push new
:- multifile changed/3. % values to the Arduino during update

term_expansion(iface(Type,PType,Vars), []) :-
    expand_vars(Vars, ReadOnly, _ReadWrite, Declarations, []),
    expand_type(Type,Methods,[]),  % Type-specific Methods
    flatten([ (:-style_check(-singleton)),
       (:- pce_begin_class(Type, PType)),
       variable(myname, name, both, "Object Name"), 
       Declarations,
       ( initialise(Self, Label:[name]) :->
	 "Initialise button and connect to device"::
	 send_super(Self, initialise(Label)),
	 send(Self, slot, myname, Label),
	 send_super(Self, slot, socket, @nil)),
       ( push(S1,N1:name) :-> "Push value to Arduino"::
                              get(S1,N1,V1),
	                      concat_atom([N1,V1],Cmd),
	                      S1 = @Npush,
	                      (send(S1,converse,Cmd)
                               -> PushStatus = succeeded
			       ;  PushStatus = failed
			      ),
	                      flog(push(Npush,N1,V1,PushStatus))
       ),
       ( pull(S2,N2:name) :-> "Pull value from Arduino"::
	                      send(S2,converse,N2),
			      get(S2,reply, Reply),
	                      parse_reply_arg1(Reply, N2, V2),
			      nonvar(N2),
			      nonvar(V2),
	                      send(S2, N2, V2),
                              !,
	                      S2 = @Npull, flog(pull(Npull,N2,V2,succeeded))
			      ; S2 = @Npull,flog(pull(Npull,N2,failed))
       ),
       ( update(US) :->
	 "Get r/o and push r/w values to Device"::
	 component(MyName,Type,US),
	 plog(updating(MyName)),
	 (offline(US) -> true
	  ; send(US,colour,colour(darkgreen)),
	    findall(P,retract(changed(US,P,_)),Ps),
	    maplist( send(US,pull), ReadOnly),
            maplist( send(US,push), Ps),
	    ( Ps = [] -> true; send(US,converse, s) )
	  %  send(US,check_level)   % PID will obviate this ?
	 ),
	 send(US,relabel),
	 plog(updated(MyName))
       ),
       ( update(US) :->
	 "Get r/o and push r/w values to Device"::
	 component(MyName,Type,US),
	 plog(update(MyName,failed))
       ),
       Methods,
       (:- pce_end_class)], List),
    maplist(format('~q.~n'),List).

expand_vars([],[],[]) --> [].
expand_vars([ro(Name,Type,Doc)|Vs], [Name|Ns], RW) -->
    [ variable(Name,Type,both,Doc) ],
    expand_vars(Vs,Ns,RW).
expand_vars([rw(Name,Type,Doc)|Vs], RO,[Name|Ns]) -->
    [ variable(Name,Type,both,Doc) ],
    expand_vars(Vs,RO,Ns).

% This check_level is a non-PID implementation.
% Mainly as an example of a generated method

expand_type(Type)   -->
    { memberchk(Type,[lagoon, cellstat]), ! },
    [(check_level(Self) :-> 
        get(Self,l,Level),
        get(Self,tl,TargetLevel),
        Amt is TargetLevel - Level,
        Error is abs(Amt),
        ( Error < 4 -> true ; adjust(l,Self,Amt) ))].

expand_type(_) --> [(check_level(@X) :-> plog(null_check_level(X)),true)].

online(Obj) :-
    get(Obj,socket,Socket),
    integer(Socket),
    Socket > -1.
    
offline(Obj) :-
    get(Obj,socket,S),
    ( S == @nil -> true ; S == -1 ),
    send(Obj,colour,colour(darkred)).

%    component(Name,_,Obj),
%    concat_atom([Name,'\nOFF LINE'], NewAtomicLabel),
%    send(Obj, label, NewAtomicLabel).
    

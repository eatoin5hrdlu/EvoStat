:- set_prolog_flag(double_quotes,codes).
:- use_module(library(pce)).
:- op(1200,xfx,':->'). % Define operators for PCE term-expansion
:- op(1200,xfx,':<-').
:- op(910, xfx,'::').
:- dynamic webValue/3.

% Generate XPCE/HTML interfaces for Arduino devices
% Arduino response to 'i' is iface(Class, Parent, Variables)
% E.g. iface(lagoon, ebutton,
%          [int(temperature,   t, ro, "Temperature"),
%           int(ttemperature, tt, rw, "Target Temperature")]).
%
% Parent class (ebutton) contains:
% rw variables: [socket, {temperature, turbidity, volume}Units ]
% and methods : [ connect, converse, parse_reply ]
% Variable myname is created in the new class

:- dynamic changed/2.   % Assertion to tell system to push new
:- multifile changed/2. % values to the Arduino during update

update_web(Obj,Var,Val) :- retractall(webValue(Obj,Var,_)),
			   assert(webValue(Obj,Var,Val)).

webgoals([], _, true).
webgoals([V|Vs], S, ( get(S,V,Val),
		      update_web(S,V,Val),
		      Goals)) :- webgoals(Vs,S,Goals).


term_expansion(iface(Type,PType,Vars), []) :-
    expand_vars(Vars, ReadOnly, ReadWrite, Declarations, []),
    webgoals(ReadWrite, S3, WebGoals),
    expand_type(Type,Methods,[]),  % Type-specific Methods
    flatten([ (:-style_check(-singleton)),
       (:- pce_begin_class(Type, PType)),
       variable(myname, name, both, "Object Name"), 
       Declarations,
       ( initialise(Self, Label:[name]) :->
	 "Initialise button and connect to device"::
	 send_super(Self, initialise(Label)),
	 send(Self, slot, myname, Label),
	 send_super(Self, slot, socket, @nil),
	 send_super(Self, connect)),
       ( push(S1,N1:name) :-> "Push value to Arduino"::
	                      get(S1,N1,V1),
			      update_web(S1,N1,V1),
                              (simulator -> true
                              ; send(S1,converse,[N1,V1])
			      )
       ),
       ( pull(S2,N2:name) :-> "Pull value from Arduino"::
	                      (simulator -> get(S2,N2,V2)
		              ; send(S2,converse,[N2]),
                                parse_reply(N2, V2),
				send(S2, N2, V2)
			      ),
			      update_web(S2,N2,V2)
       ),
       ( web_values(S3) :-> "Send r/w for web page":: WebGoals ),
       ( update(US) :->
	 "Get r/o and push r/w values to Device"::
	 (get(US,socket,@nil) -> Col=red ; Col=darkgreen),
	 send(US,colour,colour(Col)),
	 maplist( send(US,pull), ReadOnly),
	 findall(P,retract(changed(US,P)),Ps),
         maplist( send(US,push), Ps),
	 send(US, web_values),
         send(US,check_level),
	 get(US, myname, MyName),
	 format(user_error,'Updated ~s~n', MyName)
       ),
       Methods,
       (:- pce_end_class)], List),
    maplist(format('~q.~n'),List).

expand_vars([],[],[]) --> [].
expand_vars([ro(Name,Type,Doc)|Vs], [Name|Ns], RW) -->
    [ variable(Name,Type,get,Doc) ],
    expand_vars(Vs,Ns,RW).
expand_vars([rw(Name,Type,Doc)|Vs], RO,[Name|Ns]) -->
    [ variable(Name,Type,both,Doc) ],
    expand_vars(Vs,RO,Ns).

% check_level doesn't need to be here except 
% as an example of a generated method

expand_type(Type)   -->
    { memberchk(Type,[lagoon, cellstat]), ! },
    [(check_level(Self) :-> 
        get(Self,l,Level),
        get(Self,tl,TargetLevel),
        Amt is TargetLevel - Level,
        Error is abs(Amt),
        ( Error < 4 -> true ; adjust(Self,Amt) ))].

expand_type(_) --> [(check_level(_) :-> true)].


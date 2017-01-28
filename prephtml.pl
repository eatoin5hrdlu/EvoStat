:- dynamic html_syntax/0. % Also DCG version html_syntax/2.
:- dynamic webValue/3.    
:- dynamic prepped/4.

% As this code accesses the PCE objects, it can't be called from
% the web-server process to generate HTML pages.  Instead, we 
% prepare this data from the PCE object update method and assert
% it as prep/4 for efficient retrieval by the web server.
%
% DCG access to XPCE get/3 and EvoStat component/3
getx(Obj,ID)               --> {get(Obj,ID,Data),
                                refresh(Obj,ID,Data)},
                               [Data].
component(Name, Type, Obj) --> {component(Name,Type,Obj)}, [Name].

refresh(Obj,Var,Value) :-
       webValue(Obj,Var,_),
       retractall(webValue(Obj,Var,_)),
       plog(refreshing(webValue(Obj,Var,Value))),
       assert(webValue(Obj,Var,Value)),
       !.
refresh(_,_,_).

nl --> html_syntax, !, [br([])].
nl --> ['\n'].

od600 --> html_syntax, !, ['OD', sub(600)].
od600 --> ['OD600'].

% Divide by 10 and present as 4-digit float because
% Internal temperatures integral tenths of degrees C.

float_tenths(Obj,Thing) --> [Display],
    { get(Obj,Thing,InTenths),

      refresh(Obj,Thing,InTenths),
      DispTemp is float(InTenths)/10.0,
      format(atom(Display), '~4g', [DispTemp]) }.


label(temperature, Obj) --> ['Temperature '],
    float_tenths(Obj,tt), [' / '], float_tenths(Obj,t),
    getx(Obj, temperatureUnits).

label(level, Obj) --> 
                      getx(Obj,tl),
                      [' / '],
                      getx(Obj,l),
		      getx(Obj,levelUnits).

label(od, Obj) --> od600,
                   ['  .'], getx(Obj,tb),
                   ['/.'],  getx(Obj,b).

label(flow, Obj) --> ['Rate '],
                     getx(Obj,tf),
                     [' / '],
                     getx(Obj,f), [' '], getx(Obj,flowUnits).

label(supply,Name) --> component(Name,supply,Obj), nl,
                       getx(Obj, v),
                       getx(Obj, levelUnits).

label(cellstat,Name) --> 
                         component(Name,cellstat,Obj), [' '],
                         label(level,Obj),       nl,
                         label(temperature,Obj), nl,
                         label(od, Obj).

label(lagoon,Name) --> component(Name,lagoon,Obj), [' '],
                       label(level,Obj),       nl,
                       label(temperature,Obj), nl,
                       label(flow, Obj).

label(sampler,Name) -->  component(Name,sampler,Obj), nl,
    [ 'Next Level Reading in '], getx(Obj,up), ['s'], nl,
    [ 'Next Sample '], getx(Obj,ns).


prep :-                   % Prepare Data for the web page
    assert(html_syntax),
    findall(  label([id=S],Supply),
              label(supply,S,Supply,[]),Supplies),
    label(cellstat,_,Cellstat,[]),
    setof(label(id=L,Lagoon),
          label(lagoon,L,Lagoon,[]),
          Lagoons),
    label(sampler,autosampler,Sampler,[]),
    retractall(prepped(_,_,_,_)),
    assert(prepped(Supplies, Cellstat, Lagoons, Sampler)),
    retract(html_syntax),
    plog(prep(finished)).

:- use_module(library(gui_tracer)).

:- dynamic param/4.  % param(?Name, ?Type, +Slot, -Value)
:- multifile param/4.
:- dynamic webok/0.
:- multifile webok/0.
%
% These values are posted (asserted) during the update cycle
% So the webpage will show stale values if Updates are off.
%

get_label(supply, Name, [Name,br([]),Level,'%']) :-
    param(Name,supply,level,Level).

get_label(cellstat,Name,[Name,' ',Level,'%',br([]),
			 'Temperature ',TargetTemp,' / ',Temp,br([]),
			 'OD',sub(600),'  .',ODTarget,'/.',Turbidity]) :-
    param(Name,cellstat,level,Level),
    param(Name,cellstat,ttemperature,TargetTemp),
    param(Name,cellstat,temperature,Temp),
    param(Name,cellstat,tturbidity,ODTarget),
    param(Name,cellstat,turbidity,Turbidity).

get_label(lagoon,Lagoon,[Lagoon,' ',Level,'%',br([]),
			 'Temperature ',TargetTemp,' / ',Temp,
			 br([]), 'Rate 3.5/3.2']) :-
    param(Lagoon,lagoon,level,Level),
    param(Lagoon,lagoon,ttemperature,TargetTemp),
    param(Lagoon,lagoon,temperature,Temp).

get_label(sampler,autosampler, [ 'AutoSampler',br([]),
				 'Next Level Reading in 10s',br([]),
				 'Next Sample 00:20:15']).

get_label(drainage,waste,'Waste').

web_debug(Req) :-
    windows,
    !,
    write(user_error,
     'Tracing in Web page generators broken in windows'),
    nl(user_error).
    
web_debug(Req) :- 
	memberchk(search(Search),Req),
	memberchk(trace='1',Search),
	!,
	trace.
web_debug(_).

evostatName(Req, Name) :- 
    nonvar(Req),
    memberchk(search(Search),Req),
    memberchk(evostat=Name,Search),
    !.

evostatName(_Req, Name) :-
    gethostname(Fullname),
    atom_codes(Fullname,Codes),
    ( append(Root,[0'.|_],Codes) -> atom_codes(Name,Root)
    ; Name = Fullname
    ),
    write(user_error,evostat(Name)),nl(user_error).

namePlate(Name, NamePlate) :-
    concat_atom(['./images/',Name,'plate.png'],NamePlate).

semaphore :- ( webok
              -> true
              ; ( sleep(0.2), ( webok -> true ; sleep(5) )
                )
             ).

pathe(Req) :-
  web_debug(Req),
  evostatName(Req,Name),
  namePlate(Name,NamePlate),
  Title = 'Pathe Control Panel',
  semaphore,
  findall(label([id=S],Supply),get_label(supply,S,Supply),Nutrient_Inducers),
  get_label(cellstat,_,Cellstat),
  bagof(label(id=L,Lagoon),get_label(lagoon,L,Lagoon),LagoonLabels),
  get_label(sampler,autosampler,AutoSampler),
  get_label(drainage,waste,Waste),
  reply_html_page(default,
  [title(Title),
%   meta(['http-equiv'(refresh),content(5)],[]), % Make it an active page
   script([ language(javascript) ],[])],
   body([background('./images/platebglong.png')],
	[ center(img(src(NamePlate),[])),
	  div(class=supply,Nutrient_Inducers),
	  div(class=cellstat, label([],Cellstat)),
	  div([class=lagoon,width('100%')],LagoonLabels),
	  div(class=autosampler,label([],AutoSampler)),
          div(class=drainage,label([],Waste)),
	  div(class=phagestat,img(src('./mypic1.png')))]
    )% body
  ).

pathe(Request) :-
 errorPage(Request, 'Error creating EvoStat control page').



:- dynamic param/4.  % param(?Name, ?Type, +Slot, -Value)
:- multifile param/4.
:- dynamic webok/0.
:- multifile webok/0.
%
% These values are posted (asserted) during the update cycle
% So the webpage will show stale values if Updates are off.
%

get_label(supply, Name, [Name,br([]),Level,Units]) :-
    param(Name,supply,levelUnits,Units),
    param(Name,supply,level,Level).

get_label(cellstat,Name,[Name,' ',TLevel,' / ',Level,Units,br([]),
			 'Temperature ',TargetFmt,' / ',TempFmt,br([]),
			 'OD',sub(600),'  .',ODTarget,'/.',Turbidity]) :-
    param(Name,cellstat,level,Level),
    param(Name,cellstat,level_t,TLevel),
    param(Name,cellstat,levelUnits,Units),
    param(Name,cellstat,tturbidity,ODTarget),
    param(Name,cellstat,turbidity,Turbidity),
    param(Name,cellstat,temperature,Temp),
    DispTemp is float(Temp)/10.0,
    format(atom(TempFmt), '~4g', [DispTemp]),
    param(Name,cellstat,ttemperature,TargetTemp),
    DispTarget is float(TargetTemp)/10.0,
    format(atom(TargetFmt), '~4g', [DispTarget]).

get_label(lagoon,Lagoon,[Lagoon,' ',TLevel,' / ',Level,Units,br([]),
			 'Temperature ',TargFmt,' / ',TempFmt,
			 br([]), 'Rate 3.5/3.2']) :-
    param(Lagoon,lagoon,level,Level),
    param(Lagoon,lagoon,level_t,TLevel),
    param(Lagoon,lagoon,levelUnits,Units),
    param(Lagoon,lagoon,ttemperature,TargetTemp),
    param(Lagoon,lagoon,temperature,Temp),
    DispTemp is float(Temp/10.0),
    format(atom(TempFmt), '~4g', [DispTemp]),
    DispTarget is float(TargetTemp/10.0),
    format(atom(TargFmt), '~4g', [DispTarget]).

get_label(sampler,AutoSampler, [ AutoSampler,br([]),
				 'Next Level Reading in ',TimeLeft,'s',br([]),
				 'Next Sample ',TimeAtom]) :-
    param(AutoSampler,sampler,timeleft,TimeLeft),
    param(AutoSampler,sampler,nextsample,TimeAtom).

get_label(drainage,waste,'Waste').

web_debug(Req) :-
    nonvar(Req),
    memberchk(search(Search),Req),
    memberchk(trace='1',Search),
    !,
    ( windows -> 
      write(user_error,'Tracing in Web page generators broken in windows'),nl(user_error)
    ; trace
    ).
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

backPlate(Name, NamePlate) :-
    concat_atom(['./images/',Name,'.png'],NamePlate).

semaphore :- ( webok
              -> true
              ; ( sleep(0.2), ( webok -> true ; sleep(5) )
                )
             ).

plog(Term) :- write(user_error,Term),nl(user_error).

pathe(Req) :-
  web_debug(Req),
  evostatName(Req,Name),
  backPlate(Name,BackPlate),
  Title = 'Pathe Control Panel',
  semaphore,
  findall(label([id=S],Supply),get_label(supply,S,Supply),Nutrient_Inducers),
  get_label(cellstat,_,Cellstat),
  setof(label(id=L,Lagoon),get_label(lagoon,L,Lagoon),LagoonLabels),
  get_label(sampler,autosampler,AutoSamplerLabel),
  get_label(drainage,waste,Waste),
  reply_html_page(
  [title(Title),
%   meta(['http-equiv'(refresh),content(5)],[]), % Make it an active page
   script([ language(javascript) ],[])],
% ['location.reload(true)'])],
   body([background(BackPlate)],
	[
          center(a([id=logozone,href('./phagestat.pl')],i(Name))), % click
%	  div(class=phagestat,img(src('./phagestat.png'))),        % hover
	  div(class=supply,Nutrient_Inducers),

	  div(class=cellstat, label([],Cellstat)),
	  div([class=lagoon,width('100%')],LagoonLabels),
	  div(class=autosampler,label([],AutoSamplerLabel)),
          div(class=drainage,label([],Waste)),
          form([class=mod,
                action='./ipathe.pl'],
                input([type=submit,name=submit,value=change]))
        ]
    )% body
  ).

pathe(Request) :-
 errorPage(Request, 'Error creating EvoStat control page').



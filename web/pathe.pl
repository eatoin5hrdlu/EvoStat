:- dynamic param/4.  % param(?Name, ?Type, +Slot, -Value)
:- multifile param/4.
:- dynamic webok/0.
:- multifile webok/0.
%
% These values are posted (asserted) during the update cycle
% So the webpage will show stale values if Updates are off.
%

get_label(supply, Name, [Name,' ',Level,'%']) :-
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

pathe(_Request) :-
 Title = 'Pathe Control Panel',
 Name = 'darwin',
 concat_atom(['./images/',Name,'plate.png'],NamePlate),
 ( webok->true; (sleep(0.2), (webok->true;sleep(5)) ) ),
 findall(label([],Supply),get_label(supply,_S,Supply),Nutrient_Inducers),
 get_label(cellstat,_,Cellstat),
 findall(label([],Lagoon),get_label(lagoon,_L,Lagoon),LagoonLabels),
 get_label(sampler,autosampler,AutoSampler),
 get_label(drainage,waste,Waste),
 reply_html_page(default,
  [title(Title),
%   meta(['http-equiv'(refresh),content(5)],[]),
   script([ language(javascript) ],[])],
   body([background('./images/platebglong.png')],
	[ center(img(src(NamePlate),[])),
	  center(div(class=supply,Nutrient_Inducers)),
	  div(class=cellstat, label([],Cellstat)),
	  div(class=lagoon,LagoonLabels),br([clear=both]),
	  div(class=autosampler,label([],AutoSampler)),
          div(class=drainage,label([],Waste)),
	  div(class=phagestat,img(src('./images/opencvlevel.png')))]
    )% body
  ),
  write(user_error,return(pathe)),nl(user_error).

pathe(Request) :- errorPage(Request, 'Error creating PACE control page').



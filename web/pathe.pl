get_label(supply,nutrient,'Nutrient  50%').
get_label(supply,arabinose,'Arabinose  100%').
get_label(cellstat,cellstat1,'Cellstat 86%<br>Temperature 37/36<br>OD<sub>600</sub> .400/.286').
get_label(lagoon,lagoon1,'Lagoon1 30%<br>Temperature 37/36.9<br>Rate 3.5/3.2').
get_label(lagoon,lagoon2,'Lagoon2 30%<br>Temperature 37/36.9<br>Rate 3.5/3.2').
get_label(lagoon,lagoon3,'Lagoon3 30%<br>Temperature 37/36.9<br>Rate 3.5/3.2').
get_label(lagoon,lagoon4,'Lagoon4 30%<br>Temperature 37/36.9<br>Rate 3.5/3.2').
get_label(sampler,autosampler,'AutoSampler<br>Next Level Reading in 10s<br>Next Sample 00:20:15').
get_label(sampler,waste,'WASTE<br>30% full').


pathe(_Request) :-
 Title = 'Pathe Control Panel',
 Name = 'darwin',
 concat_atom(['./images/',Name,'plate.png'],NamePlate),
 findall(label(Supply),get_label(supply,_S,Supply),Nutrient_Inducers),
 get_label(cellstat,_,Cellstat),
 findall(label(Lagoon),get_label(lagoon,_L,Lagoon),LagoonLabels),
 get_label(_,autosampler,AutoSampler),
 get_label(_,waste,Waste),
 reply_html_page(evostat,
  [title(Title),
   meta(['http-equiv'(refresh),content(5)],[]),
   script([ language(javascript) ],[])],
   body([id(biologic),background('./images/platebg2.png')],
	[ center(img([src(NamePlate)],[])),
       div([class(cellstat)],Nutrient_Inducers),
       div([class(cellstat)], label(Cellstat)),
       div([class(lagoon)],LagoonLabels),
       div([class(autosampler)],label(AutoSampler)),
       div([class(autosampler)],label([style('width:200px;font-size: 120%')],Waste))]
    )% body
  ). % reply_html_page

pathe(Request) :- errorPage(Request, 'Error creating PACE control page').




nextpathe(_Req) :-                    % The web page generator
    get_background(NamePlate),
    prepped(Supplies, Cellstat, Lagoons, Sampler),
    reply_html_page( [title('Pathe Control Panel'),
     meta(['http-equiv'(refresh),content(10)],[]),% refresh
     script([ language(javascript) ],[])],
     body([background(NamePlate)],
      [center(a([id=logozone,href('./phagestat.pl')],i(NamePlate))),
       div(class=supply,Supplies),
       div(class=cellstat, label([],Cellstat)),
       div([class=lagoon,width('100%')],Lagoons),
       div(class=autosampler,label([],Sampler)),
       div(class=drainage,label([],'Waste')),
       form([class=mod,
             action='./controlpathe.pl'],
             input([type=submit,name=submit,value=change]))])).

nextpathe(Request) :- errorPage(Request,'EvoStat page error').

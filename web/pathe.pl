pathe(_Req) :-
    defaultHead('Pathe Control Panel', Head),
    pathebody(Body),
    reply_html_page(Head,Body),
    !. % det only necessary for interactive debugging

pathe(Request) :- errorPage(Request,'EvoStat page error').

pathebody(body([background(NamePlate)],
  [ center(a([id=logozone,href('./phagestat.pl')],i(NamePlate))),
    div(class=supply,Supplies),
    div(class=cellstat, label([],Cellstat)),
    div([class=lagoon,width('100%')],Lagoons),
    div(class=autosampler,label([],Sampler)),
    div(class=drainage,label([],'Waste')),
    form([class=mod,
          action='./controlpathe.pl'],
          input([type=submit,name=submit,value=change])),
  center(a([href('./plot.pl')],plotlink))]
  ) %body
  ) :- %pathebody
  backgroundImage(NamePlate),
  prepped(Supplies, Cellstat, Lagoons, Sampler).

    

pathe(_Req) :-
    defaultHead('Pathe Control Panel', Head),
    pathebody(Body),
    reply_html_page(Head,Body),
    !. % det only necessary for interactive debugging

pathe(Request) :- errorPage(Request,'EvoStat page error').

pathebody(body([background(NamePlate)],
  [ center(a([id=logozone,href('./phagestat.pl')],i('web/phagestat.jpg'))),
    div(class=supply,Supplies),
    div(class=motd, Message),
    div(class=cellstat, label([],Cellstat)),
    div([class=lagoon,width('100%')],Lagoons),
    div(class=autosampler,label([],Sampler)),
    div(class=drainage,label([],'Waste')),
    form([class=mod,
          action='./controlpathe.pl'],
          input([type=submit,name=submit,value=change])),
    center(a([href('./plot.pl')],plotlink)),
    center(a([href('./flog.txt')],flowlog)),
    center(a([href('./datalog.txt')],datalog)),
    center(a([href('./timelapse.avi')],timelapse))
  ]
  ) %body
  ) :- %pathebody
    backgroundImage(NamePlate),
    motd(MText),
    Message = [center(font([size='+5'],MText))],
    prepped(Supplies, Cellstat, Lagoons, Sampler).

    

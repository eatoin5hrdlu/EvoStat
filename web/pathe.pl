pathe(_Req) :-
    upperName(EvoStatName),
    defaultHead(EvoStatName, Head),
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
    font([size='+2'],
	 [ a([href('./phagepop.pl')],'PHAGE POPULATION MODEL'),br([],[]),
	   a([href('./flog.txt')],'Flow Log'),br([],[]),
	   a([href('./datalog.txt')],'Data Log'),br([],[]),
	   a([href('./alevel.pdf')],'OpenCV Level Sensing Paper'),br([],[]),
	   hr([],[]),
	   a([href('./ani.pl')],'Landscape Animations'),br([],[]),
	   a([href('./bani.pl')],'Animations with Black Background'),br([],[]),
	   a([href('./landscape.pdf')],'Fitness Landscape Animation Documentation'),br([],[]),
	   hr([],[]),
	   a([href('./timelapse.avi')],'Current Timelapse'),br([],[]),
	   a([href('./timelapse1417.avi')],'Previous Timelapse, Dec 12-17')])
  ]
  ) %body
  ) :- %pathebody
    backgroundImage(NamePlate),
    motd(MText),
    Message = [center(font([size='+5'],a([href('./protocol.pdf')],MText)))],
    prepped(Supplies, Cellstat, Lagoons, Sampler).

    

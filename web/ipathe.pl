ipathe(Req) :-
  evostatName(Req,Name),
  backPlate(Name,BackPlate),
  Title = 'Pathe Control Panel',
  param(CName,cellstat,ttemperature,TargetTemp),
  concat_atom([CName,'_tt'],TT),
  param(CName,cellstat,tturbidity,ODTarget),
  concat_atom(['.',ODTarget],DisplayOD),
  concat_atom([CName,'_od'],OD),
  param(AName,sampler,flowrate,Flow),
  concat_atom([AName,'_fr'],FlowRate),
  findall([ LN, ' Temperature: ',input([type=text,name=LTT,value=LV]),sup(o),'C',br([])],
	   ( param(LN,lagoon,ttemperature,LV), concat_atom([LN,'_tt'],LTT)),
           Lagoons),
  flatten([Lagoons,input([type=submit,name=submit]),
                   input([ type=button,name='Cancel',value='Cancel',
                      onClick='window.location="/web/pathe.pl"'])],
          FLagoons),
  reply_html_page(default,
  [title(Title),
   script([ language(javascript) ],[])],
   body([background(BackPlate)],
	 [form(action='./change.pl',
	       [
		'CellStat Temperature:',
                input([type=text,name=TT,value=TargetTemp]),sup(o),'C',br([]),

		'Turbidity Target: OD',sub(600),
                input([type=text,name=OD,value=DisplayOD]), br([]),

		'Flow Rate:',
                input([type=text,name=FlowRate,value=Flow]),' Volumes/Hour',br([])
                | FLagoons ] )
         ]
   )
  ).

ipathe(Request) :-
  errorPage(Request, 'Error creating Pathe Control Form').



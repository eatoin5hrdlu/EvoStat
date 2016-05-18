ipathe(Req) :-
  evostatName(Req,Name),
  backPlate(Name,BackPlate),
  Title = 'Pathe Control Panel',
  param(CName,cellstat,ttemperature,TargetTemp),
  concat_atom([CName,'_tt'],TT),
  param(CName,cellstat,tturbidity,ODTarget),
  concat_atom(['.',ODTarget],DisplayOD),
  concat_atom([CName,'_od'],OD),

  param(CName,cellstat,level_t,TargetLevel),
  concat_atom([CName,'_lv'],CL),
  param(CName,cellstat,levelUnits,CUnits),

  param(AName,sampler,flowrate,Flow),
  concat_atom([AName,'_fr'],FlowRate),
  findall([ LN, ' Temperature: ',input([type=text,name=LTT,value=LV]),&('#8530'),sup(o),'C',
	    &(nbsp),&(nbsp), &(nbsp),&(nbsp),&(nbsp),&(nbsp),
            '  Level: ', input([type=text,name=LL,value=LTarg]),LUnits,
             br([])],
	   ( param(LN,lagoon,ttemperature,LV), concat_atom([LN,'_tt'],LTT),
             param(LN,lagoon,level_t,LTarg),
             param(LN,lagoon,levelUnits,LUnits),
             concat_atom([LN,'_lv'],LL) ),
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
                input([type=text,name=TT,value=TargetTemp]),&('#8530'),sup(o),'C',
		&(nbsp),&(nbsp), &(nbsp),&(nbsp),&(nbsp),&(nbsp),
                ' CellStat Level:',
                input([type=text,name=CL,value=TargetLevel]),CUnits,br([]),

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




var_label(tt) --> [' Temperature: '].
var_label(tb) --> [' Turbidity: '].
var_label(tf) --> [' Flow Rate: '].
var_label(tl) --> [' Level : '].

var_units(tt) --> [ &('#8530'),sup(o),'C' ].
var_units(tb) --> [' OD',sub(600) ].
var_units(tf) --> [' Volumes/hour '].
var_units(tl) --> [' % full '].

space(0)  --> !, [].
space(N)  --> [&(nbsp)], {N>0, NN is N-1}, space(NN).
space(tt) --> space(4).

inputItem(Type) --> [ Name ], var_label(P),
		    { component(Name,Type,Obj),
		      concat_atom([Name,'_',P],Var),
                      webValue(Obj,P,Val)           },
		    [ input([type=text,name=Var,value=Val]) ],
		    var_units(P), nl, space(Type).

inputItems(Type) --> Is, {findall(I,inputItem(Type,I,[]),Is)}.

evoStatFields --> inputItem(cellstat), inputFields(lagoon).

controlpathe(Req) :-
    evostatName(Req,Name),
    backPlate(Name,BackPlate),
    evoStatFields(Fields,[]),
    flatten([Fields, input([type=submit,name=submit]),
                     input([ type=button,name='Cancel',value='Cancel',
                     onClick='window.location="/web/pathe.pl"'])],Inputs),
   reply_html_page([title('Pathe Control Panel'),
		    script([ language(javascript) ],[])],
		   body(background(BackPlate),
			form(action='./change.pl', Inputs ))).

controlpathe(Request) :-
  errorPage(Request, 'Error creating Pathe Control Form').



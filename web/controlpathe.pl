
var_label(tt) --> [' Temperature: '].
var_label(tb) --> [' Turbidity: '].
var_label(tf) --> [' Flow Rate: '].
var_label(tl) --> [' Level : '].

var_units(tt) --> [ &('#x2152'),sup(o),'C' ].
var_units(tb) --> [' OD',sub(600) ].
var_units(tf) --> [' Volumes/hour '].
var_units(tl) --> [' % full '].

space(0)  --> !, [].
space(N)  --> {number(N), N>0, NN is N-1}, !, [&(nbsp)], space(NN).
space(tt) --> !, space(4).
space(_) --> [].

inputItem(Type) --> [ Name ], var_label(P),
		    { component(Name,Type,Obj),
		      concat_atom([Name,'_',P],Var),
                      webValue(Obj,P,Val)
%		      (Val = 0 -> DVal = 'N/A' ; DVal=Val)
		    },
		    [ input([type=text,name=Var,value=Val]) ],
		    var_units(P), nl, space(Type).

inputItems(Type) --> {findall(I,inputItem(Type,I,[]),Is)}, Is.

evoStatFields --> inputItems(cellstat), inputItems(lagoon).

controlpathe(_Req) :-
    %   backgroundImage(BackPlate),
    ( motd(Message) -> true ; Message = 'No Status Information' ),
    assert(html_syntax),
    evoStatFields(Fields,[]),
    flatten([ input([type=text,id=motd,name=motd,value=Message]),br([]),
	      Fields, input([type=submit,name=submit,value='Submit']),
	input([ type=button,name='Cancel',value='Cancel',
		onClick='window.location="/web/pathe.pl"'])],Inputs),
    defaultHead('Pathe Control Panel',Head),
    reply_html_page(Head, body(background('/web/images/brushed.jpg'),
	     form(action='./change.pl', Inputs))),
    retract(html_syntax).

controlpathe(Request) :-
    errorPage(Request, 'EvoStat not yet ready for Web Control').



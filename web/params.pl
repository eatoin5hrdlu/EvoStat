:- use_module(library(dcg/basics)).

items([ t:372,
	tt:370,
	b:400,
	tb:400,
	f:10,
	w:500,
        v0:700,
        v1:700,
        v2:1000 ]).

html_items([]) --> [].
html_items([Var:Val|Ps]) -->
    phage_var_label(Var),
    [ input([type(text),name(Var),value(Val),style('width:30%')]) ],
    phage_var_units(Var), [br([],[])], phage_space(4),
    htmlitems(Ps).

params(_Req) :-
    items(Is),
    html_items(Is,Fields,[]),
    flatten([ Fields,
	input([type=submit,name=submit,value='Submit']),
	input([type=button,name=cancel,value='Cancel',
               onClick='window.location="/web/pathe.pl"']),
	br([],[]),
	pre([],[' ']),
	br([],[])
	],
        Inputs),
    defaultHead('Change a Micro-controller Parameter',Head),
    reply_html_page(Head,
	    body([background('/web/images/brushed.jpg'),
		  style('background-size: 100% 130%')],
	     [' ',br([],[]),
	      form([action='./change_parameter.pl'], Inputs)])).

params(Request) :-
    errorPage(Request, 'There was a problem with the Parameter page').



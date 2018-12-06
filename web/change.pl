:- dynamic web_control/1.
:- multifile web_control/1.

change(Req) :-
    backgroundImage(BackPlate),
    memberchk(search(S),Req),
    memberchk(submit=_, S),
    retractall(web_control(changeRequest(_))),
    assert(web_control(changeRequest(S))),
    plog(asserted(changeRequest)),
    reply_html_page(
	title(changes),
	body([background(BackPlate)],
	     div(class=change,
		 font([size='+5'],
		      ['Changes will take place',br([]),
		       'during next update',br([]),
		       a([href='/web/pathe.pl'],
			 'Return to EvoStat')])
		)
	    )
    ).

change(Request) :-
      errorPage(Request, 'Error changing parameters').

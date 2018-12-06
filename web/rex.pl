:- dynamic web_control/1.
:- multifile web_control/1.

rex(_Request) :-
    assert(web_control(rex)),
    backgroundImage(BackPlate),
    reply_html_page(
	title(changes),
	body([background(BackPlate)],
	     div(class=change,
		 font([size='+5'],
		      ['Restarting X11',br([]),
		       'use this link to',br([]),
		       a([href='/web/pathe.pl'],
			 'Return to EvoStat')])
		)
	    )
    ).

rex(Request) :-
   errorPage(Request, 'Error restarting EvoStat (still running).').


change(Req) :-
      evostatName(Req,Name),
      backPlate(Name,BackPlate),
      memberchk(search(S),Req),
      memberchk(submit=_, S),
      retractall(changeRequest(_)),
      assert(changeRequest(S)),
      write(user_error,asserted(changeRequest)), nl(user_error),
      reply_html_page(title(changes),
		      body([background(BackPlate)],
                       div(class=change,
		        font([size='+5'],
                        ['Changes will take place during next update',br([]),
                          a([href='/web/pathe.pl'],'Return to EvoStat')])
                       )
                      )).

change(Request) :-
      errorPage(Request, 'Error changing EvoStat parameters').

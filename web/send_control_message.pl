:- dynamic web_control/1.
:- multifile web_control/1.

send_control_message(Req) :-
    nocacheHead('Send System Message', Head),
    memberchk(search(S),Req),
    member(command=Cmd,S),
    assert(web_control(Cmd)),
    reply_html_page(Head,
		    body([],[font([size('+3')],[okay]),br([],[])])).

send_control_message(Request) :-
      errorPage(Request, 'Error asserting Web Control').





:- use_module(library(dcg/basics)).

% Add commands here, signal file appears in web directory.
commands(form([action('/web/send_control_message.pl'),method(get)],
  [
    input([type(radio),name(command),value(levelrestart)],  'Restart Level'),
    br([],[]),
    input([type(radio),name(command),value(evostatrestart)],'Restart EvoStat'),
    br([],[]),
    input([type(radio),name(command),value(xrestart)],      'Restart X'),
    br([],[]),
    input([type(radio),name(command),value(linuxrestart)],  'Restart Linux'),
    br([],[]),
    input([type=submit, name=submit,                         value='Submit']),
    br([],[]),
    input([type=button,name=cancel,                         value='Cancel',
	   onClick='window.location="/web/pathe.pl"']),
    br([],[])
  ])).
	      

kmd(_Req) :-
    defaultHead('System Control',Head),
    commands(Form),
    reply_html_page(Head,
	    body([background('/web/images/brushed.jpg'),
		  style('background-size: 100% 130%')],
	     [' ',br([],[])|Form])).

kmd(Request) :-
    errorPage(Request, 'There was a problem generating system command form').



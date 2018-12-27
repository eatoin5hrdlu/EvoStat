:- use_module(library(dcg/basics)).

% Add commands here, signal file appears in web directory.
commands(form([action('/web/send_control_message.pl'),method(get)],
  [
    input([type(radio),name(command),id(levelrestart),value(levelrestart)],[]),
    label(for(levelrestart),'Restart Level'),
    br([],[]),
    input([type(radio),name(command),id(evostatrestart),value(evostatrestart)],[]),
    label(for(evostatrestart),'Restart EvoStat'),
    br([],[]),
    input([type(radio),name(command),id(xrestart),value(xrestart)],[]),
    label(for(xrestart),'Restart X'),
    br([],[]),
    input([type(radio),name(command),id(linuxrestart),value(linuxrestart)],[]),
    label(for(linuxrestart),'Restart Linux'),
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



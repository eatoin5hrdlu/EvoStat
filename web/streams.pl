streams(_) :-
    nocacheHead('Current Open Streams',NCH),
    bagof([Src,br([],[])],
	  Mode^Str^current_stream(Src,Mode,Str),
	  Sources),
    flatten(Sources, Lines),    
    reply_html_page(default, NCH,
	  body(style('background-color:white'),
	       [pre([style='top-margin:10%'],Lines)])).

streams(Request) :-
   errorPage(Request, 'Error getting stream information').


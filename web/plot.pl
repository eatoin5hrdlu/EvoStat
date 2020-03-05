plot(_Req) :-
    defaultHead('Population Plot', Head),
%   shell('web/ptemp.py flog.txt'),
    reply_html_page(Head,body(background('phagepop.png'),[])),
    !. % det only necessary for interactive debugging

plot(Request) :- errorPage(Request,'EvoStat Plot page error').


    

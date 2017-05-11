plot(_Req) :-
    defaultHead('Level/Temperature Plot', Head),
    shell('web/ptemp.py flog.txt'),
    reply_html_page(Head,body(background('plot.png'),[])),
    !. % det only necessary for interactive debugging

plot(Request) :- errorPage(Request,'EvoStat Plot page error').


    

:- use_module(library(process),[process_create/3]).

octaveParameters([])            --> [].
octaveParameters([X=Y|Ps])      -->
    ['\n# name: ', X, '\n# type: global scalar\n', Y, '\n' ],
    octaveParameters(Ps).

run_model(Req) :-
    memberchk(search(S),Req),
    select(submit=_, S,S1),
    octaveParameters(S1,Tokens,[]),
    flatten(Tokens,List),
    open('./web/ES_params.txt',write,F),
    maplist(write(F),List),
    close(F),
    process_create('/usr/bin/octave',
		   ['--path', './web', './web/modrun.m'],
		   [process(PID)]),
    process_wait(PID, Exit, [timeout(10)]),
    defaultHead('Population Plot', Head),
    reply_html_page(Head,body(background('phagepop.png'),[pre([],Exit)])).

run_model(Request) :-
      errorPage(Request, 'Error changing parameters').





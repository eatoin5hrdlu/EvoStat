:- use_module(library(process),[process_create/3]).

octaveParameters([])            --> [].
octaveParameters([X=Y|Ps])      -->
    { atom_concat('.',_,Y)->atomic_list_concat(['0',Y],NY);Y=NY},
    ['\n# name: ', X, '\n# type: global scalar\n', NY, '\n' ],
    octaveParameters(Ps).

run_model(Req) :-
    memberchk(search(S),Req),
    select(submit=_, S,S1),
    octaveParameters(S1,Tokens,[]),
    flatten(Tokens,List),
    open('./web/ES_params.txt',write,F),
    maplist(write(F),List),
    close(F),
    Plot = 'phagepop.png',
    (exists_file(Plot) -> delete_file(Plot); true),
    process_create('/usr/bin/octave',
		   ['--path', './web', './web/modrun.m'],
		   [process(PID)]),
    process_wait(PID, Exit, [timeout(10)]),
    nocacheHead('Population Plot', Head),
    timestring(Time),
    reply_html_page(Head,body(background(Plot),
			[font([size('+3')],[Time,'  ', 'Exit Code ', Exit]),
			       br([],[])])).

run_model(Request) :-
      errorPage(Request, 'Error changing parameters').





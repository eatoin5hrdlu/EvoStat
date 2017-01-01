:- ensure_loaded(library(pce)).
:- dynamic wdbcount/2.
:- assert(wdbcount(0,0)).

dblog(A) :-
    retract(wdbcount(N,Start)),
    (Start == 0 -> open(weblog,write,Stream) ; Start = Stream),
    NN is N + 1,
    format(Stream, '~q:~q~n',[N,A]),
    flush_output(Stream),
    assert(wdbcount(NN,Stream)).

newpathe(_Req) :-                    % The web page generator
    gethostname(Fullname),
    atomic_list_concat([Name|_],'.',Fullname),
    concat_atom(['./images/',Name,'.png'],NamePlate),
    dblog(NamePlate),
    findall(  label([id=S],Supply),
              label(supply,S,Supply,[]),
              Nutrient_Inducers),
    dblog(cellstat0),
    label(cellstat,_,Cellstat,[]),
    dblog(cellstat2),
    setof(label(id=L,Lagoon),
          label(lagoon,L,Lagoon,[]),
          LagoonLabels),
    label(sampler,autosampler,AutoSamplerLabel,[]),
    reply_html_page( [title('Pathe Control Panel'),
     meta(['http-equiv'(refresh),content(5)],[]),% refresh
     script([ language(javascript) ],[])],
     body([background(NamePlate)],
      [center(a([id=logozone,href('./phagestat.pl')],i(Name))),
       div(class=supply,Nutrient_Inducers),
       div(class=cellstat, label([],Cellstat)),
       div([class=lagoon,width('100%')],LagoonLabels),
       div(class=autosampler,label([],AutoSamplerLabel)),
       div(class=drainage,label([],'Waste')),
       form([class=mod,
             action='./ipathe.pl'],
             input([type=submit,name=submit,value=change]))])).

newpathe(Request) :- errorPage(Request,'EvoStat page error').

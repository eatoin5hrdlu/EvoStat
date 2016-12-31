% DCG access to XPCE get/3 and EvoStat component/3
    
getx(Obj,ID)            -->{get(Obj,ID,Data)},        [Data].
component(Name,Type,Obj)-->{component(Name,Type,Obj)},[Name].

nl --> {html,!}, [br([])].
nl --> ['\n'].

% Divide by 10 and present as 4-digit float because
% Internal temperatures integral tenths of degrees C.

float_tenth(Obj,Thing) --> [Display],
    { get(Obj,Thing,InTenths),
      DispTemp is float(InTenths)/10.0,
      format(atom(Display), '~4g', [DispTemp]) }.

label(temperature, Obj) --> ['Temperature '],
    float_tenths(Obj,tt), [' / '], float_tenths(Obj,t),
    getx(Obj, temperatureUnits).

label(level, Obj) --> getx(Obj,lt),
                      [' / '],
                      getx(Obj,l), getx(Obj,levelUnits).

label(od, Obj) --> ['OD',sub(600)],
                   ['  .'], getx(Obj,tb),
                   ['/.'],  getx(Obj,b).

label(flow, Obj) --> ['Rate '],
                     getx(Obj,tf),
                     [' / '],
                     getx(Obj,f), getx(Obj,flowUnits).

label(supply,Name) --> component(Name,supply,Obj), nl,
                       getx(Obj, l),
                       getx(Obj, levelUnits).

label(cellstat,Name) --> component(Name,cellstat,Obj), [' '],
                         label(level,Obj),       nl,
                         label(temperature,Obj), nl,
                         label(od, Obj).

label(lagoon,Name) --> component(Name,cellstat,Obj), [' '],
                       label(level,Obj),       nl,
                       label(temperature,Obj), nl,
                       label(flow, Obj).

label(sampler,Name) -->  component(Name,autosampler,Obj), nl,
    [ 'Next Level Reading in '], getx(Obj,rt), ['s'], nl,
    [ 'Next Sample '], getx(Obj,ns).

newpathe(_Req) :-                    % The web page generator
    gethostname(Fullname),
    atomic_list_concat([Name|_],'.',Fullname),
    concat_atom(['./images/',Name,'.png'],NamePlate),
    findall(  label([id=S],Supply),
              label(supply,S,Supply,[]),
              Nutrient_Inducers),
    label(cellstat,_,Cellstat,[]),
    setof(label(id=L,Lagoon),
          label(lagoon,L,Lagoon,[]),
          LagoonLabels),
    label(sampler,autosampler,AutoSamplerLabel,[]),
    reply_html_page(
    [title('Pathe Control Panel'),
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
             input([type=submit,name=submit,value=change]))
     ])% end of body
  ).

newpathe(Request) :- errorPage(Request,'EvoStat page error').

%
% Good dynamic term format for generating CSV files for ploting
% Use this to create the entire octave plot.m file.
% Data, color assignments, legends, etc.
% colororder = [ [r1,g1,b1], [r2,g2,b2] ... ]
% set(gca, 'ColorOrder', colororder)
% hold on
% plot(array)

% text(xpos,ypos,'Title','fontsize',32,'color',colororder(1));
% Label should be to the right of the graph and line up with
% the last value of the corresponding data
%
octave_label(F,Margin,Data,Color,Text) :-
    setof(D,member(D,Data), AllData),
    append(_,[Max],AllData),
    format(F,'text(~p,~p,~q,"fontsize",20,"color",~p);',
                  [Margin,Max,Text,Color]).

toCSV(Time,d(TmVal,BVal,HLval,LTmVal,LLval,Hout,Lout)) :-
   temperature(host0,Time,TmVal),
   turbidity(host0, Time, BVal),
   level(host0, Time, HLval),
   temperature(lagoon2,Time,LTmVal),
   level(lagoon2,Time,LLval),
   hostValve(autosampler,Time,Hout),
   lagoonValve(autosampler,Time,Lout).


data_sequence([],_).
data_sequence([_Time:d(Tm1,B1,HL1,Tm2,LL1,O1,O2)|Rest], F) :-
    format(F,'~p, ~p, ~p, ~p, ~p, ~p, ~p\n',
	    [Tm1, B1, HL1,Tm2,LL1,O1, O2]),
    data_sequence(Rest, F).


test :-
    consult('datalog.txt'),
    setof(Time:Data, toCSV(Time,Data), AllData),
    open(httltloo,write,F),
    data_sequence(AllData,F),
    close(F).

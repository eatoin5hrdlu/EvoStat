:- dynamic start/1.

process(File,Outfile) :-
    open(File,read,Stream),
    tell(Outfile),
    read_process(Stream, 1, host0, [lagoon2]),
    told.

read_process(Stream, Num, First, Others) :-
    read_term(Stream, Term, []),
    process_term(Term, First, Others, Num, Next),
    flush_output,
    !,
    read_process(Stream, Next, First, Others).
read_process(_Stream, _Num, _First, _Others).

delta_time(Time, Delta) :-
    start(STime),
    !,
    Delta is Time - STime.
delta_time(Time, 0) :-
    assert(start(Time)).

process_term(end_of_file, _, _, _, _) :- !, fail.
process_term(temperature(Name,Time,Value), First, Rest, Num, Next) :-
    DegreesC is Value/10.0,
    (Name = First ->
	 delta_time(Time, Delta),
 	 writeln(''), write(Delta),
	 write(','), write(DegreesC),
	 Next is Num + 1
     ;   memberchk(Name,Rest),
	 write(','), write(DegreesC),
	 Next = Num
    ),
    !.
process_term(_Term, _First, _Rest, Num, Num).


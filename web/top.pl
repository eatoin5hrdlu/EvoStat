top(_) :-
  nocacheHead('Top Memory Users',NCH),
  process_create('/usr/bin/top', ['-b','-n','1','-o','%MEM'],
		 [ stdout(pipe(Out)),
		   env(['COLUMNS'=400,'LINES'=200])
		 ]),
  read_lines(Out, Lines),
  reply_html_page(default, NCH,
	  body(style('background-color:white'),
	       [pre([style='top-margin:10%'],Lines)])).

top(Request) :-
   errorPage(Request, 'Error creating top output').

read_lines(Out, Lines) :-
        read_line_to_codes(Out, Line1),
        read_lines(Line1, Out, Lines).

read_lines(end_of_file, _, []) :- !.
read_lines(Codes, Out, [Line,br([],[])|Lines]) :-
        atom_codes(Line, Codes),
        read_line_to_codes(Out, Line2),
        read_lines(Line2, Out, Lines).

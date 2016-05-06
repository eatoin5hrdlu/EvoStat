:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_client)).

:- dynamic( [ load_time/2 ] ).

:- multifile user:head//2.

user:head(_, Head) -->       % CSS: Add to every page
    html(head([
link([ type('text/css'), rel('stylesheet'), href('/open/css/default.css') ]),
link([ type('text/css'), rel('stylesheet'), href('/open/css/template.css') ]),
link([ type('text/css'), rel('stylesheet'), href('/open/css/mtemplate.css') ]),
link([ type('text/css'), rel('stylesheet'), href('/open/css/content.css') ]),
link([ type('text/css'), rel('stylesheet'), href('/open/css/mcontent.css') ]),
link([ type('text/css'), rel('stylesheet'), href('/open/css/print.css') ])
	 | Head ])).

emit_file(Path,Options,Request) :-
        message('REQUEST: ~q~n', [Request]),
	http_read_data(Request, Data, []),
	message('POST DATA: ~q~n', [Data]),
	http_reply_file(Path,Options,Request).

process_files(WildCard, Security) :-
	expand_file_name(WildCard, Files),
	add_handlers(Files, Security).

png_handler(Path, Request):-
        message('PNG Handler for ~q~n~n',[Path]),
	http_reply_file(Path,[mime_type(image/png)],Request).

gif_handler(Path, Request):-
        message('GIF Handler for ~q~n~n',[Path]),
	http_reply_file(Path,[mime_type(image/gif)],Request).

jpg_handler(Path, Request):-
        message('JPG Handler for ~q~n~n',[Path]),
	http_reply_file(Path,[mime_type(image/jpg)],Request).

add_handlers([],_).
add_handlers([Path|T],Options) :-
	concat_atom(['/',Path], URL),
	file_base_name(Path, Base),
	file_name_extension(Root, Ext, Base),
	( add_handler(Ext, URL, Root, Path, Options)
	  -> message('Created a handler for ~w~n',[URL])
	  ; message('Did not create a handler for ~w~n',[URL])
	),
	add_handlers(T, Options).

add_handler(pl, URL, Root, Path, Options) :-
	ensure_loaded(Path),
        time_file(Path,Time),
        assert(load_time(Path,Time)),
	http_handler(URL, Root, Options).

add_handler(html, URL, _Root, Path, Options) :-
	http_handler(URL, http_reply_file(Path,[]), Options).

add_handler(txt, URL, _Root, Path, Options) :-
	http_handler(URL,http_reply_file(Path,[mime_type(text/plain)]),Options).
add_handler(png, URL, _Root, Path, Options) :-
	http_handler(URL,png_handler(Path),Options).

add_handler(gif, URL, _Root, Path, Options) :-
	http_handler(URL,gif_handler(Path),Options).

add_handler(jpg, URL, _Root, Path, Options) :-
	http_handler(URL,jpg_handler(Path),Options).

add_handler(jpeg, URL, _Root, Path, Options) :-
	http_handler(URL,jpg_handler(Path),Options).

add_handler(js, URL, _Root, Path, Options) :-
	http_handler(URL,http_reply_file(Path,[mime_type(text/javascript)]),Options).

add_handler(css, URL, _Root, Path, Options) :-
	http_handler(URL,http_reply_file(Path,[mime_type(text/css)]),Options).

%
% CGI stuff to process parameter lists
% for POST or GET HTTP Requests.
%

read_n_codes(0, _,  [] ) :- !.
read_n_codes(N, S,[C|T]) :- get_code(S,C), NN is N-1, read_n_codes(NN,S,T).

get_parameters(Request, Parameters) :-
    get_parameter_codes(Request, Codes),
    parse_parameters(Parameters, Codes, []).

get_parameter_codes(Request, Codes) :- % read POST parameters from Stream
    member(method(post), Request),
    !,
    member(input(Stream),Request),
    member(content_length(Length),Request),
    read_n_codes(Length, Stream, Codes).

get_parameter_codes(Request, Codes) :-  % GET parameters are in Request
    member(request_uri(URI),Request),
    atom_codes(URI, Codes).

parse_parameters([H|T]) -->
    [P1], parse_attribute(P1, PCodes),
    [V1], parse_value(V1, VCodes),
    { name(Param, PCodes),
      name(Value, VCodes),
      functor(H,Param,1), arg(1,H,Value)
    },
    !,
    parse_parameters(T).
parse_parameters([]) --> [].

parse_value(0'&, []) --> !.
parse_value(C,[C|T]) --> [NC], !, parse_value(NC,T).
parse_value(C,  [C]) --> [].  % No & after last parameter

parse_attribute(0'=,[])  --> !.
parse_attribute(C,[C|T]) --> [NC], parse_attribute(NC,T).

message_list([]).
message_list([H|T]) :- message('~q~n', [H]), message_list(T).


% Javascript function and timeout call to return to the referring page

js_return(Request, Function, 'setTimeout("goback()",4000)' ) :-
  member(referer(Whence),Request),
  concat_atom(['function goback(){window.location="',Whence,'";}'],Function).


errorPage(Request, Message) :-
    js_return(Request, Function, Flyback),
    reply_html_page( biologic,
		[ title('An Error has occurred'),
		  script([language(javascript)],[Function])],
		  body([onLoad(Flyback) ],
	          center(h1(Message)))
		   ).

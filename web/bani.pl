bani(_Req) :-
    defaultHead('Including Mutation Labels',Default),
    style(Style),
    toggle(Toggle,'blabel.gif','bplain.gif'),
    Head = [ Style, Toggle | Default ],
    reply_html_page(Head, body([],
	[ img([ id(timg),
		onclick('toggleimage("timg");'),
		src('/web/images/bplain.gif'),width('80%')],[]) ])).

bani(Request) :-
    errorPage(Request, 'Animation not available').

style(style('body { background-color: #000000; }')).



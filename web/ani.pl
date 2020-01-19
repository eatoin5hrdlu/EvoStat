ani(_Req) :-
    defaultHead('Evolution Landscape Display',Default),
    Style = style('body { background-color: #FFFFFF; }'),
    toggle(Toggle,'label.gif','plain.gif'),
    Head = [ Style, Toggle | Default ],
    reply_html_page(Head, body([],
	[ img([ id(timg),
		onclick('toggleimage("timg");'),
		src('/web/images/plain.gif'),width('80%')],[]) ])).

ani(Request) :-
    errorPage(Request, 'Animation not available').






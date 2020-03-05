
% Problems with image cacheing, we always want to see the latest
phagestat(_) :-
  nocacheHead('Inside Phagestat',NCH),
  reply_html_page(default,
		  [meta(['http-equiv'(refresh),content(5)],[])|NCH],
	  body(style('background-color:grey'),
	       [pre([style='top-margin:5%'],[' ']),
		center(img([src('./phagestat.jpg'),width('90%')],[]))
	       ])).

phagestat(Request) :-
    errorPage(Request, 'Error creating PhageStat image').




% Problems with image cacheing, we always want to see the latest
phagestat(_) :-
  nocacheHead('Inside Phagestat',NCH),
  reply_html_page(default, NCH,
	  body(style('background-color:grey'),
	       [pre([style='top-margin:10%'],[' ']),
		center(img([src('./phagestat.jpg'),width('96%')],[])),
		video([width(320),height(240),autoplay],
		   source([src('./timelapse.avi'),type('video/x-msvideo')],[]))
	       ])).

phagestat(Request) :-
    errorPage(Request, 'Error creating PhageStat image').


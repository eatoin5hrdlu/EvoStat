
phagestat(_) :-
  reply_html_page(default,  [
    title('Inside Phagestat'),
    meta(['http-equiv'(refresh),content(90)],[]), % Make it an active page
    script([ language(javascript) ],[])],
    body([background('./phagestat.jpg')],[])).

phagestat(Request) :-
    errorPage(Request, 'Error creating PhageStat image').



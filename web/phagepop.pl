% Model variables
% dc
% h0
% ec
% ad
% dur
% vol
% lsl
% bl
%
% To add user-settable variable to the model:
% You will be editing three files:
% The text file ES_params.txt, 
% the Prolog file phagepop.pl, and
% the Octave/Matlab model in modrun.m (and local.m for testing)
%
% Steps:
% 1) Add the new variable to ES_params.txt
% 2) Add rules for phage_var_label//1 and phage_var_units//1 in phagepop.pl
% 3) Add code to use variable in modrun.m (and local.m for testing)

% Get values from the current ES_param file
%
:- use_module(library(dcg/basics)).

read_parameters(Ps) :-
    read_file_to_codes('/home/peter/src/EvoStat/web/ES_params.txt',Codes,[]),
    parse_params(Ps,Codes,[]).

white_space --> [C], {C < 33,!}, white_space.
white_space --> [].
		
parse_params(Ps) --> "# name: ", [C], !, parse_name(Cs),
		     { atom_codes(Name,[C|Cs]) },
		     white_space,
		     parse_val(Name, Ps).

parse_params(Ps) --> [_], !, parse_params(Ps).
parse_params([]) --> [].

parse_name([C|Cs]) --> [C], {char_type(C,alnum),!}, parse_name(Cs).
parse_name([])     --> [].

parse_val(Name, [Name:Value|Ps]) -->
    "# type: global scalar", !,
    white_space,
    number(Value),
    parse_params(Ps).
parse_val(Name, Ps) --> [_], parse_val(Name,Ps).

phage_var_label(ad)  --> [ 'Adsorption Rate : '].
phage_var_label(bl)  --> [ 'Baseline for Y axis : '].
phage_var_label(ec)  --> [ 'Eclipse Interval : '].
phage_var_label(dur)  --> [ 'Simulation Length : '].
phage_var_label(fr) -->  [' Flow Rate : '].
phage_var_label(vol) --> [' Lagoon Volume : '].
phage_var_label(h0) --> [' Host Cell Concentration: '].
phage_var_label(p0) --> [' Initial Phage Population: '].
phage_var_label(pp) --> [' Phage Production: '].
phage_var_label(kg) --> !, [' E. coli Doubling Time : '].
phage_var_label(moi) --> [' Multiplicity of Infection : '].
phage_var_label(lsl) --> [' Linear=1, Semilog=0 : '].
phage_var_label(K)  --> { concat_atom([k,X],K) }, [' Growth Factor ',X,' : '].

% Variable prefix determines some parameter types:
% kXXX   is a growth constant
% tmXXX  a temperature in deg C
phage_var_units(ad) --> [ 'mL/min' ].
phage_var_units(bl) --> [ 'Population' ].
phage_var_units(kg) --> [ 'min' ].
phage_var_units(dur) --> [ 'hours' ].
phage_var_units(ec) --> [ 'min' ].
phage_var_units(fr) --> [ 'Volumes/hour' ].
phage_var_units(vol) --> [ 'mL' ].
phage_var_units(h0) --> [ 'Cells/mL' ].
phage_var_units(p0) --> [ 'Virons/mL' ].
phage_var_units(pp) --> [' Phage/Cell-hour '].
phage_var_units(moi) --> [' Phage/Cell '].
phage_var_units(lsl) --> [' 1 or 0 '].
phage_var_units(KX) --> { concat_atom([k,_],KX),! }, ['t',sup(-1)].
phage_var_units(TM) --> { concat_atom([tm,_],TM),! }, [&('#x2152'),sup(o),'C'].
phage_var_units(OD) --> { concat_atom([od,_],OD),! }, [' OD',sub(600) ].

phage_space(0)  --> !, [].
phage_space(N)  --> {number(N), N>0, NN is N-1}, !, [&(nbsp)], phage_space(NN).
phage_space(tt) --> !, phage_space(4).
phage_space(_) --> [].

modelItems([]) --> [].
modelItems([Var:Val|Ps]) -->
    phage_var_label(Var),
    [ input([type(text),name(Var),value(Val),style('width:30%')]) ],
    phage_var_units(Var), [br([],[])], phage_space(4),
    modelItems(Ps).

phagepop(_Req) :-
    read_parameters(Ps),
    assert(html_syntax),
    modelItems(Ps,Fields,[]),
    retract(html_syntax),
    flatten([ Fields,
	input([type=submit,name=submit,value='Submit']),
	input([type=button,name=cancel,value='Cancel',
               onClick='window.location="/web/pathe.pl"']),
	br([],[]),
	pre([],[' ']),
	br([],[]),
	font([size('+1')],
	['Be patient, E.g. 300 hour simulation requires more than 1 minute on 2.1Ghz CPU'])],
        Inputs),
    defaultHead('Pathe Population Model',Head),
    reply_html_page(Head,
	    body([background('/web/images/brushed.jpg'),
		  style('background-size: 100% 130%')],
	     [' ',br([],[]),
	      center(a([href('/web/model.txt'),style('color:white')],
		    [font([size('+2')],'CLICK HERE TO SEE MODEL SOURCE')])),
	      center(font([size('+2'),style('color:#FDFDFD')],
			  [' ',br([],[])])),
	      form([action='./run_model.pl'], Inputs)])).

phagepop(Request) :-
    errorPage(Request, 'There was a problem generating the model').



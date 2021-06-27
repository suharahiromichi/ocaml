#
#
#

lexer.ml:	lexer.mll
	ocamllex lexer.mll

parser.ml:	parser.mly
	ocamlyacc parser.mly

myc.top:	lexer.ml parser.ml syntax.ml
	ocamlmktop syntax.ml parser.mli parser.ml lexer.ml -o myc.top

interpret:	lexer.ml parser.ml syntax.ml interpret.ml
	ocamlmktop syntax.ml parser.mli parser.ml lexer.ml interpret.ml -o interpret

clean:;
	rm -f myc.top lexer.ml parser.ml
	rm -f lexer.mli lexer.mlo lexer.cmi lexer.cmo
	rm -f parser.mli parser.mlo parser.cmi parser.cmo
	rm -f syntax.mli syntac.mlo syntax.cmi syntax.cmo

# END

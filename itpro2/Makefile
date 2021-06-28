#
#
#

lexer.ml:	lexer.mll
	ocamllex lexer.mll

parser.ml:	parser.mly
	ocamlyacc parser.mly

interpret:	lexer.ml parser.ml syntax.ml interpret.ml
	ocamlmktop syntax.ml parser.mli parser.ml lexer.ml interpret.ml -o interpret

clean:;
	rm -f myc.top interpret compile
	rm -f lexer.ml parser.ml
	rm -f sum.ml sum
	rm -f *.mli *.mlo *.cmi *.cmo

# END

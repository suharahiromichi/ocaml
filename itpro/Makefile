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

compile:	lexer.ml parser.ml syntax.ml compile.ml
	ocamlmktop syntax.ml parser.mli parser.ml lexer.ml compile.ml -o compile

test:
	./compile -no-version -noprompt < sum.myc > sum.ml
	ocamlc sum.ml -o sum

clean:;
	rm -f myc.top interpret compile
	rm -f lexer.ml parser.ml
	rm -f sum.ml sum
	rm -f *.mli *.mlo *.cmi *.cmo

# END

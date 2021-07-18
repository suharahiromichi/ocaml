#
# Makefile
#

PROGRAM = main

INCS = risc.cmi

OBJS = risc.cmo main.cmo

###

OC = ocamlc -g

all:
	make $(PROGRAM)

clean:
	rm -f $(OBJS) $(INCS) $(PROGRAM).cmi

.SUFFIXES: .mli .ml .cmi .cmo

.mli.cmi:
	$(OC) -c $<

.ml.cmo:
	$(OC) -c $<

$(PROGRAM): $(INCS) $(OBJS)
	$(OC) $(OBJS) $(LDFLAGS) $(LIBS) -o $(PROGRAM)

# END

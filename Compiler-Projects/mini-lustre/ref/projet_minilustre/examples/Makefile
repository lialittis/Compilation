MINILUSTRE=../compiler/minilustre

FILE=test_when.mls
NODE=main_when
FILE_ML=`basename $(FILE) .mls`.ml

compile:
	$(MINILUSTRE) -verbose -main $(NODE) $(FILE)
	ocamlc -o $(NODE) -thread unix.cma threads.cma graphics.cma $(FILE_ML)

clean:
	rm -f *.cm* $(FILE_ML) $(NODE) *~

cleanall: clean
	rm -f *~


#MINILUSTRE=/Users/pouzet/cours/ens/synchrone/18-19/tp-minilustre/correction/minilustre
MINILUSTRE=../compiler/minilustre

FILE=test.mls
NODE=test
FILE_ML=`basename $(FILE) .mls`.ml

compile:
	$(MINILUSTRE) -main $(NODE) $(FILE)
	ocamlc -o $(NODE) -thread unix.cma threads.cma graphics.cma $(FILE_ML)

clean:
	rm -f *.cm* $(FILE_ML) $(NODE) *~

cleanall: clean
	rm -f *~


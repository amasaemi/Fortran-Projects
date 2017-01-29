FC=gfortran
CFLAGS=-Wall -std=f2008ts -fimplicit-none -ftree-vectorizer-verbose=2 -Wno-maybe-uninitialized -static-libgfortran -flto

all:
	$(FC) $(CFLAGS) -o bin/exec src/source.f90

clean:
	rm -f bin/*.*

run:
	rm -f ./bin/output.txt
	cd ./bin; ./exec;
	cat ./bin/output.txt

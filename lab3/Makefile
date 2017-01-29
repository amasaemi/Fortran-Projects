FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto
FOPT=-O3 -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/file_io.f90 -J obj/ -o obj/file_io.o
	$(FC) $(FFLAGS) $(FOPT) -c src/file_process.f90 -J obj/ -o obj/file_process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/source.f90 -I obj/ -o obj/source.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/exec obj/environment.o obj/file_process.o obj/file_io.o obj/source.o

clean:
	rm -f obj/*
	rm -f bin/*

run:
	rm -f ./bin/F3.txt
	cd ./bin; ./exec;
	cat ./bin/F3.txt

FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto
FOPT=-O3 -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/people_io.f90 -J obj/ -o obj/people_io.o
	$(FC) $(FFLAGS) $(FOPT) -c src/people_process.f90 -J obj/ -o obj/people_process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/source.f90 -I obj/ -o obj/source.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/source obj/environment.o obj/people_process.o obj/people_io.o obj/source.o

clean:
	rm -f obj/*
	rm -f bin/*

run:
	cd ./bin; ./source
	cat ./bin/output.txt

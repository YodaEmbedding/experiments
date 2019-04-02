FC=gfortran
FFLAGS=-O3 -fcheck=all -fimplicit-none -fdefault-real-8
FFLAGS+=-Wall -Wno-real-q-constant -Wno-conversion
LDFLAGS=-llapack
PYTHON=python

.PHONY: clean default run

default: run

clean:
	rm -rf *.mod *.o \
		q1 \
		results*.csv \
		plot*.png \
		plot*.svg

run: q1
	./q1

q1: q1.o integrator.o utils.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

q1.o: q1.f90 integrator.o utils.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

integrator.o: integrator.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

utils.o: utils.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

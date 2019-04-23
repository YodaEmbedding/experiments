FC=gfortran
FFLAGS=-O3 -fcheck=all -fimplicit-none -fdefault-real-8
FFLAGS+=-Wall -Wno-real-q-constant -Wno-conversion
LDFLAGS=-llapack
PYTHON=python

.PHONY: clean default run

default: run

clean:
	rm -rf *.mod *.o \
		spectral \
		results/ \
		plots/

run: spectral
	mkdir -p results plots
	./spectral

spectral: spectral.f90 utils.o integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

spectral.o: spectral.f90 utils.o integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

integrator.o: integrator.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

utils.o: utils.f90 integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

FC=gfortran
FFLAGS=-O3 -fcheck=all -fimplicit-none -fdefault-real-8
FFLAGS+=-Wall -Wno-real-q-constant -Wno-conversion
LDFLAGS=-llapack
PYTHON=python

.PHONY: clean default run

default: run

clean:
	rm -rf *.mod *.o \
		shoot \
		spectral \
		results*.csv \
		plot*.png \
		plot*.svg

run: shoot spectral
	./shoot
	./spectral

shoot: shoot.o integrator.o utils.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

shoot.o: shoot.f90 integrator.o utils.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

spectral: spectral.f90 utils.o integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

spectral.o: spectral.f90 utils.o integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

integrator.o: integrator.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

utils.o: utils.f90 integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

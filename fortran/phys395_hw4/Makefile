FC=gfortran
FFLAGS=-O3 -fcheck=all -fimplicit-none -fopenmp
FFLAGS+=-Wall -Wno-real-q-constant -Wno-conversion
LDFLAGS=-llapack -lcfitsio
PYTHON=python

.PHONY: clean default run

default: run

clean:
	rm -rf *.mod *.o \
		phys395_hw4_ode \
		results*.csv \
		plot*.fit \
		plot*.mp4 \
		plot*.png \
		plot*.svg

run: phys395_hw4_ode
	./$<

phys395_hw4_ode: phys395_hw4_ode.o fitsio.o integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

phys395_hw4_ode.o: phys395_hw4_ode.f90 fitsio.o integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

integrator.o: integrator.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

fitsio.o: fitsio.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

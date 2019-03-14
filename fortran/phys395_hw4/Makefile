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
		plot*.mp4 \
		plot*.svg

run: phys395_hw4_ode
	./$<
	# $(PYTHON) plot.py

perf_test: phys395_hw4_ode
	time ./$<
	# $(PYTHON) plot.py --disable-animation

phys395_hw4_ode: phys395_hw4_ode.o fitsio.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

phys395_hw4_ode.o: phys395_hw4_ode.f90 fitsio.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

fitsio.o: fitsio.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

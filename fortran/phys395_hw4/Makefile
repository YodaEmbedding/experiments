CC=gfortran
CFLAGS=-Wall -O0 -fcheck=all -fimplicit-none -fdefault-real-16
LDFLAGS=-llapack
PYTHON=python

.PHONY: clean default run

default: run

clean:
	rm -rf *.mod *.o \
		phys395_hw4_ode \
		results*.csv \
		plot*.svg

run: phys395_hw4_ode
	./$<
	$(PYTHON) plot.py

phys395_hw4_ode: phys395_hw4_ode.o
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@

phys395_hw4_ode.o: phys395_hw4_ode.f90
	$(CC) $(CFLAGS) $(LDFLAGS) -c $< -o $@

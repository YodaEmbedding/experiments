CC=gfortran
CFLAGS=-Wall -O3 -fcheck=all -fimplicit-none -fdefault-real-8
LDFLAGS=-llapack
PYTHON=python

.PHONY: clean default run_phys395_hw3_optimization

default: run_phys395_hw3_optimization

clean:
	rm -rf *.mod *.o \
		phys395_hw3_optimization \
		results*.csv \
		plot*.png

run_phys395_hw3_optimization: phys395_hw3_optimization
	./phys395_hw3_optimization
	$(PYTHON) plot.py

phys395_hw3_optimization: phys395_hw3_optimization.o
	$(CC) $(CFLAGS) $(LDFLAGS) phys395_hw3_optimization.o -o phys395_hw3_optimization

phys395_hw3_optimization.o: phys395_hw3_optimization.f90
	$(CC) $(CFLAGS) $(LDFLAGS) -c phys395_hw3_optimization.f90

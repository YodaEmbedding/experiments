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

phys395_hw3_optimization: q1_roots.o q3_minima.o q5_fit.o phys395_hw3_optimization.o
	$(CC) $(CFLAGS) $(LDFLAGS) q1_roots.o q3_minima.o q5_fit.o phys395_hw3_optimization.o -o phys395_hw3_optimization

phys395_hw3_optimization.o: phys395_hw3_optimization.f90
	$(CC) $(CFLAGS) $(LDFLAGS) -c phys395_hw3_optimization.f90 -o phys395_hw3_optimization.o

q1_roots.o: q1_roots.f90
	$(CC) $(CFLAGS) $(LDFLAGS) -c q1_roots.f90

q3_minima.o: q3_minima.f90
	$(CC) $(CFLAGS) $(LDFLAGS) -c q3_minima.f90

q5_fit.o: q5_fit.f90
	$(CC) $(CFLAGS) $(LDFLAGS) -c q5_fit.f90

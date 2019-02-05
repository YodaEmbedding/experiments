CC=gfortran
CFLAGS=-Wall -O3 -fdefault-real-8
LDFLAGS=-llapack
PYTHON=python

.PHONY: clean default run_phys395_hw2_curve_fitting

default: run_phys395_hw2_curve_fitting

clean:
	rm -rf *.mod *.o \
		phys395_hw2_curve_fitting orthogonal_polynomials \
		results_*.csv \
		plot_*.png

run_phys395_hw2_curve_fitting: phys395_hw2_curve_fitting
	./phys395_hw2_curve_fitting
	# $(PYTHON) plot.py

phys395_hw2_curve_fitting: phys395_hw2_curve_fitting.o
	$(CC) $(CFLAGS) $(LDFLAGS) phys395_hw2_curve_fitting.o -o phys395_hw2_curve_fitting

phys395_hw2_curve_fitting.o: phys395_hw2_curve_fitting.f90
	$(CC) $(CFLAGS) $(LDFLAGS) -c phys395_hw2_curve_fitting.f90

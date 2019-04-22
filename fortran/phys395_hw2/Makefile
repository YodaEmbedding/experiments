CC=gfortran
CFLAGS=-Wall -O3 -fdefault-real-8 -fcheck=all
LDFLAGS=-llapack
PYTHON=python

.PHONY: clean default run

default: run

clean:
	rm -rf *.mod *.o \
		phys395_hw2_curve_fitting \
		results_*.csv \
		plot_*.png

run: phys395_hw2_curve_fitting data.dat
	./phys395_hw2_curve_fitting
	$(PYTHON) plot.py

data.dat:
	curl -L https://www.dropbox.com/s/s3cccki89u7whqx/phys395-data.dat -o data.dat

phys395_hw2_curve_fitting: phys395_hw2_curve_fitting.o
	$(CC) $(CFLAGS) $(LDFLAGS) phys395_hw2_curve_fitting.o -o phys395_hw2_curve_fitting

phys395_hw2_curve_fitting.o: phys395_hw2_curve_fitting.f90
	$(CC) $(CFLAGS) $(LDFLAGS) -c phys395_hw2_curve_fitting.f90

CC=gfortran
CFLAGS=-Wall -O3 -fdefault-real-8

.PHONY: clean default run_test_gauss_jordan run_orthogonal_polynomials

default: run_orthogonal_polynomials

clean:
	rm -rf *.mod *.o \
		test_gauss_jordan orthogonal_polynomials \
		results_*.csv \
		plot_*.png

run_orthogonal_polynomials: orthogonal_polynomials
	./orthogonal_polynomials
	python3 plot.py
	#
	# TODO verify this all works in the VM (clone it on your Archlinux PC)

run_test_gauss_jordan: test_gauss_jordan
	./test_gauss_jordan

orthogonal_polynomials: gauss_jordan.o orthogonal_polynomials.o
	$(CC) $(CFLAGS) gauss_jordan.o orthogonal_polynomials.o -o orthogonal_polynomials

test_gauss_jordan: gauss_jordan.o test_gauss_jordan.o
	$(CC) $(CFLAGS) gauss_jordan.o test_gauss_jordan.o -o test_gauss_jordan

gauss_jordan.o: gauss_jordan.f90
	$(CC) $(CFLAGS) -c gauss_jordan.f90

orthogonal_polynomials.o: phys395_hw1_orthogonal_polynomials.f90
	$(CC) $(CFLAGS) -c phys395_hw1_orthogonal_polynomials.f90 -o orthogonal_polynomials.o

test_gauss_jordan.o: test_gauss_jordan.f90
	$(CC) $(CFLAGS) -c test_gauss_jordan.f90

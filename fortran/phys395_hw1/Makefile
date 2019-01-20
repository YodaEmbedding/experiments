CC=gfortran
CFLAGS=-Wall -O3 -fdefault-real-8

.PHONY: clean default run_gauss_jordan_test

default: run_gauss_jordan_test

clean:
	rm -rf *.mod *.o a.out gauss_jordan_test

run_gauss_jordan_test: gauss_jordan_test
	./gauss_jordan_test

gauss_jordan_test: gauss_jordan.o gauss_jordan_test.o
	$(CC) $(CFLAGS) gauss_jordan.o gauss_jordan_test.o -o gauss_jordan_test

gauss_jordan.o: gauss_jordan.f90
	$(CC) $(CFLAGS) -c gauss_jordan.f90

gauss_jordan_test.o: gauss_jordan_test.f90
	$(CC) $(CFLAGS) -c gauss_jordan_test.f90

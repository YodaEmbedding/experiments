FC=gfortran
FFLAGS=-O3 -fcheck=all -fimplicit-none -fopenmp
FFLAGS+=-Wall -Wno-real-q-constant -Wno-conversion
FR8FLAGS=$(FFLAGS)
FR8FLAGS+=-fdefault-real-8
LDFLAGS=-llapack -lcfitsio
PYTHON=python

.PHONY: clean default run

default: run

clean:
	rm -rf *.mod *.o \
		q2 \
		q3 \
		integrator_r8.f90 \
		results*.csv \
		plot*.fit \
		plot*.mp4 \
		plot*.png \
		plot*.svg

run: q2 q3
	./q2
	./q3

q2: q2.o integrator_r8.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

q3: q3.o fitsio.o integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

q2.o: q2.f90 integrator_r8.o
	$(FC) $(FR8FLAGS) $(LDFLAGS) -c $< -o $@

q3.o: q3.f90 fitsio.o integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

fitsio.o: fitsio.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

integrator.o: integrator.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

integrator_r8.o: integrator_r8.f90
	$(FC) $(FR8FLAGS) $(LDFLAGS) -c $< -o $@

# Unholy hackery... pure evil
integrator_r8.f90: integrator.f90
	cp integrator.f90 integrator_r8.f90
	sed -i 's/module integrator/module integrator_r8/g' integrator_r8.f90

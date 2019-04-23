FC=gfortran
FFLAGS=-O3 -fcheck=all -fimplicit-none -fdefault-real-8
FFLAGS+=-Wall -Wno-real-q-constant -Wno-conversion
LDFLAGS=-llapack
PYTHON=python

.PHONY: clean default run

default: run

clean:
	rm -rf *.mod *.o \
		q1 \
		q2 \
		q3 \
		results/ \
		plots/
	cd q5 && make clean && cd ..

run: q1 q2 q3 q5/spectral
	mkdir -p results plots
	./q1
	./q2
	./q3
	./q5/spectral
	cp q5/plots/* plots
	cp q5/results/* results

q1: q1.o utils.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

q1.o: q1.f90 utils.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

q2: q2.o utils.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

q2.o: q2.f90 utils.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

q3: q3.o utils.o integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

q3.o: q3.f90 utils.o integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

q5/spectral:
	cd q5 && make && cd ..

shoot: shoot.o integrator.o utils.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

shoot.o: shoot.f90 integrator.o utils.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

spectral: spectral.f90 utils.o integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

spectral.o: spectral.f90 utils.o integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

integrator.o: integrator.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

utils.o: utils.f90 integrator.o
	$(FC) $(FFLAGS) $(LDFLAGS) -c $< -o $@

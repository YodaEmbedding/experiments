CXX=g++
CXXFLAGS=-O2
CXXFLAGS+=-Wall -Wextra -pedantic
LDFLAGS=

.PHONY: clean default run

default: run

clean:
	rm -rf *.o \
		main

run: main
	./main

main: main.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

main.o: main.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -c $< -o $@

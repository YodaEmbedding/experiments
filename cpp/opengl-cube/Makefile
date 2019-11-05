CXX=g++
CXXFLAGS=-std=c++1z -O2
CXXFLAGS+=-Wall -Wextra -pedantic
LDFLAGS=-lglut -lGLEW -lGLU -lGL

.PHONY: clean default run cmake cmake_run

default: run

clean:
	rm -rf *.o \
		main \
		_build

run: main
	./main

cmake_run: cmake
	./_build/main

cmake:
	mkdir -p _build
	cd _build && cmake .. && make && cp main ..

main: main.o shader.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

main.o: main.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -c $< -o $@

%.o: %.cpp %.h
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -c $< -o $@

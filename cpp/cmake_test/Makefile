.PHONY: all clean

all:
	mkdir -p build
	cd build && cmake .. && make

run: all
	./build/main

clean:
	rm -rf build

.PHONY: all clean run

all: ./obj_dir/Vfibonacci

clean:
	rm -rf obj_dir *.o

run: all
	./obj_dir/Vfibonacci

./obj_dir/Vfibonacci: fibonacci.sv fibonacci.cpp
	verilator -Wall --cc fibonacci.sv --exe fibonacci.cpp
	make -C obj_dir -f Vfibonacci.mk Vfibonacci CXXFLAGS='--std=c++11 -Wall' LIBS=''

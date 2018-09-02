TARGET=fibonacci

.PHONY: all clean run

all: $(TARGET)

clean:
	rm -rf obj_dir *.o $(TARGET)

run: all
	./obj_dir/Vfibonacci

# Commit this then follow exact example Makefile? (Or manually execute commands)

$(TARGET): main.cpp ./obj_dir/Vfibonacci
	g++ -o '$@' main.cpp

./obj_dir/Vfibonacci: fibonacci.sv
	verilator -Wall --cc fibonacci.sv --exe fibonacci.cpp
	make -C obj_dir -f Vfibonacci.mk Vfibonacci CXXFLAGS='--std=c++11 -Wall' LIBS='-lSDL2'

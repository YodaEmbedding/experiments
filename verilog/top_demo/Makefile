TARGET=./obj_dir/Vtop
.PHONY: all clean run

all: $(TARGET)

clean:
	rm -rf obj_dir *.o $(TARGET)

run: all
	./obj_dir/Vtop

./obj_dir/Vtop: top.sv main.cpp
	verilator -Wall --cc top.sv --exe main.cpp
	make -C obj_dir -f Vtop.mk Vtop CXXFLAGS='--std=c++11 -Wall' LIBS=''

# $(TARGET): ./obj_dir/Vtop main.cpp
# 	g++ -o '$@' main.cpp

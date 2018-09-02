#include <iostream>
#include "verilated.h"
#include "Vfibonacci.h"

int main() {
    int time = 0;

    auto top = new Vfibonacci();
    top->a = 0;
    top->b = 1;

    while (!Verilated::gotFinish() && time < 8) {
        top->clk = 0; top->eval();
        top->clk = 1; top->eval();
        time++;

        std::cout << top->a << std::endl;
    }

    return 0;
}

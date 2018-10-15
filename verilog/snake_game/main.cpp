#include <iostream>
#include "verilated.h"
#include "Vtop.h"

int main() {
    int time = 0;

    auto top = new Vtop();

    while (!Verilated::gotFinish() && time < 8) {
        top->clk = 0; top->eval();
        top->clk = 1; top->eval();
        time++;

        std::cout << top->fib << std::endl;
    }

    return 0;
}

#include <verilated.h>
#include "Vday04.h"
#include <iostream>
#include <vector>
#include <string>
#include <cstring>

int main(int argc, char** argv) {
    Verilated::commandArgs(argc, argv);

    bool part2 = false;
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--part2") == 0) part2 = true;
    }

    std::vector<char> grid;
    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.empty()) continue;
        for (char c : line) {
            grid.push_back(c);
        }
    }

    Vday04* dut = new Vday04;

    dut->clock = 0;
    dut->clear = 1;
    dut->start = 0;
    dut->part2 = part2 ? 1 : 0;
    dut->in_valid = 0;
    dut->in_char = 0;

    auto cycle = [&]() {
        dut->clock = 1;
        dut->eval();
        dut->clock = 0;
        dut->eval();
    };

    cycle();
    dut->clear = 0;
    cycle();

    dut->start = 1;
    cycle();
    dut->start = 0;

    size_t pos = 0;
    dut->in_valid = 0;
    dut->in_char = 0;

    while (!dut->out_valid) {
        if (dut->in_ready && pos < grid.size()) {
            dut->in_valid = 1;
            dut->in_char = static_cast<uint8_t>(grid[pos]);
            pos++;
        } else {
            dut->in_valid = 0;
            dut->in_char = 0;
        }
        cycle();
    }

    std::cout << dut->out_count << std::endl;

    delete dut;
    return 0;
}

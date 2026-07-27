#include <jac/machine/compiler/qjsBcOpcodes.h>

#include <cstddef>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <span>
#include <sstream>
#include <string_view>
#include <vector>


int main(const int argc, const char* argv[]) {
    for (size_t i = 0; i < OP_COUNT; ++i) {
        std::cout << i << " : " << opcodeNames[i] << std::endl;
    }
    return 0;
}

#include <iostream>
#include <string>

#include "day04_common.h"
#include "day04_part1.h"
#include "day04_part2.h"

static void print_usage(const char* argv0) {
  std::cerr << "Usage:\n"
               "  "
            << argv0
            << " --part1 < input.txt\n"
               "  "
            << argv0
            << " --part2 < input.txt\n"
               "\n"
               "Notes:\n"
               "  - Exactly one of --part1/--part2 must be provided.\n";
}

int main(int argc, char** argv) {
  std::ios::sync_with_stdio(false);
  std::cin.tie(nullptr);

  bool part1 = false;
  bool part2 = false;

  for (int i = 1; i < argc; ++i) {
    const std::string arg = argv[i];
    if (arg == "--part1") {
      part1 = true;
    } else if (arg == "--part2") {
      part2 = true;
    } else if (arg == "--help" || arg == "-h") {
      print_usage(argv[0]);
      return 0;
    } else {
      std::cerr << "Unknown argument: " << arg << "\n\n";
      print_usage(argv[0]);
      return 2;
    }
  }

  if (part1 == part2) {  // both true or both false
    std::cerr << "Error: choose exactly one of --part1 or --part2.\n\n";
    print_usage(argv[0]);
    return 2;
  }

  const Grid g = read_grid(std::cin);
  if (g.h == 0 || g.w == 0) return 0;

  const std::int64_t ans = part1 ? solve_part1(g) : solve_part2(g);
  std::cout << ans << '\n';
  return 0;
}



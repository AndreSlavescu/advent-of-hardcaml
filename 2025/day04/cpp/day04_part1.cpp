#include "day04_part1.h"

#include <cstdint>

std::int64_t solve_part1(const Grid& g) {
  const auto neighbor_sum = convolve_8_neighbors(g);
  std::int64_t accessible = 0;
  const std::size_t n = g.cell.size();
  for (std::size_t i = 0; i < n; ++i) {
    if (g.cell[i] && neighbor_sum[i] < 4) ++accessible;
  }
  return accessible;
}

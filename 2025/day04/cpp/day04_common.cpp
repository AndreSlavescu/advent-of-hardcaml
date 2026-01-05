#include "day04_common.h"

#include <iostream>
#include <string>
#include <vector>

Grid read_grid(std::istream& in) {
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(in, line)) {
    if (!line.empty() && line.back() == '\r') line.pop_back();
    if (!line.empty()) lines.push_back(line);
  }

  Grid g;
  if (lines.empty()) return g;

  g.h = static_cast<int>(lines.size());
  g.w = static_cast<int>(lines[0].size());
  g.cell.assign(g.h * g.w, 0);

  for (int r = 0; r < g.h; ++r)
    for (int c = 0; c < g.w; ++c)
      g.cell[grid_index(r, c, g.w)] = (lines[r][c] == '@') ? 1 : 0;

  return g;
}

std::vector<std::uint8_t> convolve_8_neighbors(const Grid& g) {
  std::vector<std::uint8_t> out(g.h * g.w, 0);

  auto get = [&](int r, int c) -> int {
    return (r >= 0 && r < g.h && c >= 0 && c < g.w) ? g.cell[grid_index(r, c, g.w)] : 0;
  };

  for (int r = 0; r < g.h; ++r) {
    for (int c = 0; c < g.w; ++c) {
      int sum = get(r-1, c-1) + get(r-1, c) + get(r-1, c+1)
              + get(r,   c-1)              + get(r,   c+1)
              + get(r+1, c-1) + get(r+1, c) + get(r+1, c+1);
      out[grid_index(r, c, g.w)] = sum;
    }
  }

  return out;
}

#include "day04_common.h"

#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>

static bool is_roll(char ch) { return ch == '@'; }

Grid read_grid(std::istream& in) {
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(in, line)) {
    if (!line.empty() && line.back() == '\r') line.pop_back();  // Windows CRLF
    if (line.empty()) continue;
    lines.push_back(line);
  }

  Grid g;
  if (lines.empty()) return g;

  g.h = static_cast<int>(lines.size());
  g.w = static_cast<int>(lines[0].size());

  g.cell.assign(static_cast<std::size_t>(g.h) * static_cast<std::size_t>(g.w), 0);
  for (int r = 0; r < g.h; ++r) {
    for (int c = 0; c < g.w; ++c) {
      g.cell[static_cast<std::size_t>(grid_index(r, c, g.w))] = is_roll(lines[r][c]) ? 1 : 0;
    }
  }
  return g;
}

static inline std::uint8_t get_cell(const Grid& g, int r, int c) {
  if (r < 0 || r >= g.h || c < 0 || c >= g.w) return 0;
  return g.cell[static_cast<std::size_t>(grid_index(r, c, g.w))];
}

std::vector<std::uint8_t> convolve_8_neighbors(const Grid& g) {
  std::vector<std::uint8_t> out(static_cast<std::size_t>(g.h) * static_cast<std::size_t>(g.w), 0);

  static constexpr int k8nbr[3][3] = {{1, 1, 1}, {1, 0, 1}, {1, 1, 1}};

  for (int r = 0; r < g.h; ++r) {
    for (int c = 0; c < g.w; ++c) {
      int sum = 0;
      for (int kr = 0; kr < 3; ++kr) {
        for (int kc = 0; kc < 3; ++kc) {
          const int k = k8nbr[kr][kc];
          if (k == 0) continue;
          sum += k * static_cast<int>(get_cell(g, r + (kr - 1), c + (kc - 1)));
        }
      }
      out[static_cast<std::size_t>(grid_index(r, c, g.w))] = static_cast<std::uint8_t>(sum);
    }
  }

  return out;
}

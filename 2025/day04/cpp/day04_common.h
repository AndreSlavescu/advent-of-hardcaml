#pragma once

#include <cstdint>
#include <istream>
#include <vector>

// Simple bitmap values ('.' -> 0, '@' -> 1).
struct Grid {
  int h = 0;
  int w = 0;
  std::vector<std::uint8_t> cell; // H * W
};

Grid read_grid(std::istream& in);

// 8-neighbor count via convolution with kernel:
//   1 1 1
//   1 0 1
//   1 1 1
//
// Returns a vector of size h*w with values in [0,8].
std::vector<std::uint8_t> convolve_8_neighbors(const Grid& g);

inline int grid_index(int r, int c, int w) { return r * w + c; }

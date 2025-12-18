#include "day04_part2.h"

#include <cstdint>
#include <queue>
#include <vector>

std::int64_t solve_part2(const Grid& g) {
  // Part 2 repeatedly removes accessible rolls (<4 neighbors) until stable.
  // This is equivalent to peeling the 4-core of the 8-neighbor adjacency graph.
  std::vector<std::uint8_t> alive = g.cell;
  std::vector<std::uint8_t> deg = convolve_8_neighbors(g);

  std::queue<int> q;
  const int n = g.h * g.w;
  for (int i = 0; i < n; ++i) {
    if (alive[static_cast<std::size_t>(i)] && deg[static_cast<std::size_t>(i)] < 4) q.push(i);
  }

  static const int dr[8] = {-1, -1, -1, 0, 0, 1, 1, 1};
  static const int dc[8] = {-1, 0, 1, -1, 1, -1, 0, 1};

  std::int64_t removed = 0;
  while (!q.empty()) {
    const int i = q.front();
    q.pop();

    if (!alive[static_cast<std::size_t>(i)]) continue;
    if (deg[static_cast<std::size_t>(i)] >= 4) continue;

    alive[static_cast<std::size_t>(i)] = 0;
    ++removed;

    const int r = i / g.w;
    const int c = i % g.w;

    for (int k = 0; k < 8; ++k) {
      const int rr = r + dr[k];
      const int cc = c + dc[k];
      if (rr < 0 || rr >= g.h || cc < 0 || cc >= g.w) continue;
      const int j = grid_index(rr, cc, g.w);
      if (!alive[static_cast<std::size_t>(j)]) continue;

      auto& d = deg[static_cast<std::size_t>(j)];
      if (d > 0) --d;
      if (d < 4) q.push(j);
    }
  }

  return removed;
}



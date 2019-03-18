#include <algorithm>
#include <limits>
#include <map>
#include <numeric>
#include <string>
#include <vector>

int main() {
  using dist_t = uint16_t;
  char from[21];
  char to[21];
  dist_t dist;

  auto *dists = static_cast<dist_t *>(malloc(8 * 8 * sizeof(dist_t)));
  int loc_count = 0;
  while (true) {
    auto get_index = [&loc_count](const std::string &loc) {
      static std::map<std::string, int> locs;
      auto iter = locs.find(loc);
      if (iter == locs.end()) {
        locs[loc] = loc_count;
        loc_count = locs.size();
        return loc_count - 1;
      }
      return iter->second;
    };

    auto rv = scanf("%20s to %20s = %d\n", from, to, &dist);

    if (rv != 3) {
      break;
    }

    auto ifrom = get_index(from);
    auto ito = get_index(to);
    dists[(ifrom << 3) + ito] = dist;
  }

  std::vector<short> v(loc_count);
  std::iota(v.begin(), v.end(), 0);

  int min_dist = std::numeric_limits<int>::max();
  int max_dist = 0;
  do {
    if (v.front() < v.back()) {
      int result = 0;
      for (int i = 0; i < loc_count - 1; i++) {
        result += dists[v[i] < v[i + 1] ? (v[i] << 3) + v[i + 1]
                                        : (v[i + 1] << 3) + v[i]];
      }
      if (result < min_dist) {
        min_dist = result;
      }
      if (result > max_dist) {
        max_dist = result;
      }
    }
  } while (std::next_permutation(v.begin(), v.end()));

  printf("%d %d\n", min_dist, max_dist);
  free(dists);
}

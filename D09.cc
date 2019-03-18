#include <algorithm>
#include <iostream>
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>

struct index {
  int from;
  int to;
};

bool operator==(const index &fst, const index &snd) {
  return fst.from == snd.from && fst.to == snd.to;
}

template <> struct std::hash<index> {
  typedef index argument_type;
  typedef std::size_t result_type;
  result_type operator()(argument_type const &s) const noexcept {
    result_type const h1(std::hash<int>{}(s.from));
    result_type const h2(std::hash<int>{}(s.to));
    return h1 ^ (h2 << 1);
  }
};

int main() {
  char from[21];
  char to[21];
  int dist;

  std::unordered_map<index, int> dists;
  int loc_count = 0;
  while (true) {
    auto get_index = [&loc_count](const std::string &loc) {
      static std::unordered_map<std::string, int> locs;
      auto iter = locs.find(loc);
      if (iter == locs.end()) {
        locs[loc] = loc_count;
        loc_count = locs.size();
      }
      return locs[loc];
    };

    auto rv = scanf("%20s to %20s = %d\n", from, to, &dist);

    if (rv != 3) {
      break;
    }

    auto ifrom = get_index(from);
    auto ito = get_index(to);
    dists[{ifrom, ito}] = dist;
  }

  std::vector<int> v(loc_count);
  std::generate(v.begin(), v.end(), []() {
    static int i = 0;
    return i++;
  });

  int min_dist = std::numeric_limits<int>::max();
  int max_dist = 0;
  do {
    if (v.front() < v.back()) {
      int result = 0;
      for (int i = 0; i < loc_count - 1; i++) {
        auto from = v[i];
        auto to = v[i + 1];
        if (from < to) {
          result += dists[{from, to}];
        } else {
          result += dists[{to, from}];
        }
      }
      if (result < min_dist) {
        min_dist = result;
      }
      if (result > max_dist) {
        max_dist = result;
      }
    }
  } while (std::next_permutation(v.begin(), v.end()));

  std::cout << "Min: " << min_dist << std::endl;
  std::cout << "Max: " << max_dist << std::endl;
}

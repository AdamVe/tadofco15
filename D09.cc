#include <algorithm>
#include <iostream>
#include <limits>
#include <vector>

int main() {
  std::vector<int> v = {0, 1, 2, 3, 4, 5, 6, 7};

  int dist[8][8] = {
    {   0, 129,  58,  13,  24,  60,  71,  67},
    { 129,   0, 142,  15, 135,  75,  82,  54},
    {  58, 142,   0, 118, 122, 103,  49,  97},
    {  13,  15, 118,   0, 116,  12,  18,  91},
    {  24, 135, 122, 116,   0, 129,  53,  40},
    {  60,  75, 103,  12, 129,   0,  15,  99},
    {  71,  82,  49,  18,  53,  15,   0,  70},
    {  67,  54,  97,  91,  40,  99,  70,   0}
  };

  int min_dist = std::numeric_limits<int>::max();
  int max_dist = 0;
  do {
    if (v.front() < v.back()) {
      int result = 0;
      for (int i = 0; i < 7; i++) {
        result += dist[v[i]][v[i + 1]];
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

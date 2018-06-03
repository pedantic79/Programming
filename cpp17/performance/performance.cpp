#include <algorithm>
#include <benchmark/benchmark.h>
#include <list>
#include <random>
#include <vector>

using namespace std;

template <typename T, typename S>
void ordered_insert(T &container, const S &input) {
    for (const auto &n : input) {
        auto itr = begin(container);
        for (; itr != end(container); itr++) {
            if (n < *itr) {
                break;
            }
        }
        container.insert(itr, n);
    }
}

auto seed = random_device{}();

template <typename T>
void BM_Insert(benchmark::State &state) {
    using S = typename T::value_type;
    vector<S> input(state.range(0));
    mt19937 gen{seed};
    generate(begin(input), end(input), gen);

    for (auto _ : state) {
        // Don't time construction of object.
        state.PauseTiming();
        T container;
        state.ResumeTiming();

        ordered_insert(container, input);
    }
}

BENCHMARK_TEMPLATE(BM_Insert, vector<uint32_t>)->Range(1, 1 << 15);
BENCHMARK_TEMPLATE(BM_Insert, list<uint32_t>)->Range(1, 1 << 15);

BENCHMARK_MAIN();

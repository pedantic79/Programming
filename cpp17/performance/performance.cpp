#include <algorithm>
#include <chrono>
#include <iostream>
#include <list>
#include <random>
#include <sstream>
#include <vector>

using namespace std;

struct Timer {
    Timer() : start_(clock_::now()) {}
    void reset() { start_ = clock_::now(); }
    auto elapsed() const {
        return chrono::duration_cast<second_>(clock_::now() - start_).count();
    }

  private:
    using clock_ = chrono::high_resolution_clock;
    using second_ = chrono::duration<double, ratio<1>>;
    chrono::time_point<clock_> start_;
};

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

template <typename T, typename S>
auto run(const vector<S> &input, int iterations) {
    Timer t;
    vector<decltype(t.elapsed())> r(iterations);

    generate(begin(r), end(r), [&]() -> auto {
        T container;
        t.reset();
        ordered_insert(container, input);
        return t.elapsed();
    });

    return r;
}

template <typename T>
string stats(const T &v) {
    const auto sum = accumulate(begin(v), end(v), 0.0);
    const auto m = sum / v.size();

    const auto sqsum =
        accumulate(begin(v), end(v), 0.0, [=](const auto acc, const auto x) {
            return acc + (x - m) * (x - m);
        });

    stringstream oss;
    oss << fixed << " mean: " << m << " stdev: " << sqrt(sqsum / v.size())
        << '\n';
    return oss.str();
}

template <typename T>
T get_default(const vector<T> &v, int n, const T &d) {
    T r = d;

    if (n < v.size()) {
        T num = v[n];
        r = (num == 0) ? d : num;
    }

    return r;
}

template <typename T>
auto parse_argv(int argc, char const *argv[]) {
    const vector<string> args(argv + 1, argv + argc);
    vector<T> argi;
    transform(begin(args), end(args), back_inserter(argi),
              [](const auto &s) -> T {
                  stringstream ss(s);
                  T i;
                  ss >> i;

                  return i;
              });
    return argi;
}

int main(int argc, char const *argv[]) {
    const auto argi = parse_argv<int>(argc, argv);
    const auto len = get_default(argi, 0, 10000);
    const auto times = get_default(argi, 1, 5);

    vector<uint32_t> input(len);

    mt19937 gen{random_device{}()};
    generate(begin(input), end(input), gen);

    using T = decltype(input)::value_type;
    const auto times_l = run<list<T>>(input, times);
    const auto times_v = run<vector<T>>(input, times);

    cout << input.size() << " integers " << times << " times\n";
    cout << "list  :" << stats(times_l);
    cout << "vector:" << stats(times_v);
    return 0;
}

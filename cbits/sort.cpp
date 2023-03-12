#include <bits/stdc++.h>
#include <cstdint>

extern "C" {
    void cplusplus_sort(int64_t* xs, int len) {
        std::sort(xs, xs + len);
    }
}

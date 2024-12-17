[![build](https://github.com/sergv/vector-quicksort/actions/workflows/haskell-ci.yaml/badge.svg)](https://github.com/sergv/vector-quicksort/actions/workflows/haskell-ci.yaml)

# Synopsis

This package features reasonable sort function that is a good default
for sorting mutable vectors using well-known quicksort algorithm.
During development one of the goals was making it perform on par with
C++’s `std::sort`, i.e. fast.

While providing reasonably good single-threaded default sorting, the
algorithm in this package is also parallelisable and can provide ways
to run on multiple cores using either threads or sparks, so bigger
vectors can be sorted even faster.

# Algorithm details

Technically it’s an [introsort](https://en.wikipedia.org/wiki/Introsort), i.e.
pathological `O(N^2)` quicksort case will get delegated to heapsort when recursion
gets too deep, thus making algorithm reliably `O(N * log(N))`.

# Benchmarks

The benchmark is to generate some random arrays of different length
and sort them. For each array size N two versions are generated: one
from range [0, N] (`with few duplicates` below),
another from [0, N / 1000] (`with many duplicates` below) to
introduce duplicates. And there’s one reasonably big array (around
40,000 elements) with lots of duplicates that I managed to come by in
the real world under `Sorting fuzzy matching scores vector` that comes
from `test.txt` file in this repository. with

Test setup: AMD Ryzen 9 7950X3D 16-Core Processor, Intel i5-9600K CPU @ 3.70GHz, GHC 9.10.1.

The default sort exported in `Data.Vector.Algorithms.Quicksort` is
`Sequential` and `Median3or5`.

NB `vector-algorithms` benchmark is not included below (but it’s still
there and will run for you locally) because it doesn’t specialise by
default and in that case takes around 100 times more time than C++.
That’s not representative, when this library doesn’t specialise it
suffers similar slowdown (please refer to the docs on how to alleviate
that).

```
$ cabal run bench -- -j1 --time-mode wall --timeout 300 -p '!/vector-algorithms heapsort/'
All
  Sorting fuzzy matching scores vector
    C++:                              OK
      228  μs ±  22 μs
    Sequential ST Median3:            OK
      104  μs ± 6.1 μs, 0.45x
    Sequential IO Median3:            OK
      104  μs ± 6.0 μs, 0.46x
    ParStrategies ST Median3:         OK
      102  μs ± 5.6 μs, 0.45x
    ParStrategies IO Median3:         OK
      104  μs ± 5.6 μs, 0.46x
    Sequential ST Median3or5:         OK
      94.6 μs ± 5.7 μs, 0.41x
    Sequential IO Median3or5:         OK
      96.7 μs ± 5.5 μs, 0.42x
    ParStrategies ST Median3or5:      OK
      95.0 μs ± 5.6 μs, 0.42x
    ParStrategies IO Median3or5:      OK
      95.9 μs ± 5.9 μs, 0.42x
    Threads IO Median3:               OK
      104  μs ± 8.2 μs, 0.45x
    Threads IO Median3or5:            OK
      98.0 μs ± 5.5 μs, 0.43x
    fallback heapsort:                OK
      1.53 ms ±  97 μs, 6.70x
  Int64
    Sorting 10 random arrays of length 16 with few duplicates
      C++:                            OK
        673  ns ±  65 ns
      Sequential ST Median3:          OK
        442  ns ±  21 ns, 0.66x
      Sequential IO Median3:          OK
        432  ns ±  37 ns, 0.64x
      ParStrategies ST Median3:       OK
        437  ns ±  38 ns, 0.65x
      ParStrategies IO Median3:       OK
        450  ns ±  21 ns, 0.67x
      Sequential ST Median3or5:       OK
        439  ns ± 7.4 ns, 0.65x
      Sequential IO Median3or5:       OK
        439  ns ±  26 ns, 0.65x
      ParStrategies ST Median3or5:    OK
        460  ns ±  43 ns, 0.68x
      ParStrategies IO Median3or5:    OK
        454  ns ±  35 ns, 0.67x
      Threads IO Median3:             OK
        1.23 μs ± 103 ns, 1.83x
      Threads IO Median3or5:          OK
        1.22 μs ±  95 ns, 1.81x
      fallback heapsort:              OK
        911  ns ±  66 ns, 1.35x
    Sorting 10 random arrays of length 17 with few duplicates
      C++:                            OK
        688  ns ±  66 ns
      Sequential ST Median3:          OK
        561  ns ±  45 ns, 0.82x
      Sequential IO Median3:          OK
        566  ns ±  55 ns, 0.82x
      ParStrategies ST Median3:       OK
        690  ns ±  55 ns, 1.00x
      ParStrategies IO Median3:       OK
        679  ns ±  62 ns, 0.99x
      Sequential ST Median3or5:       OK
        588  ns ±  38 ns, 0.85x
      Sequential IO Median3or5:       OK
        590  ns ±  48 ns, 0.86x
      ParStrategies ST Median3or5:    OK
        646  ns ±  64 ns, 0.94x
      ParStrategies IO Median3or5:    OK
        645  ns ±  24 ns, 0.94x
      Threads IO Median3:             OK
        1.45 μs ± 133 ns, 2.11x
      Threads IO Median3or5:          OK
        1.40 μs ± 129 ns, 2.03x
      fallback heapsort:              OK
        989  ns ±  59 ns, 1.44x
    Sorting 10 random arrays of length 100 with few duplicates
      C++:                            OK
        4.48 μs ± 368 ns
      Sequential ST Median3:          OK
        5.05 μs ± 444 ns, 1.13x
      Sequential IO Median3:          OK
        5.02 μs ± 367 ns, 1.12x
      ParStrategies ST Median3:       OK
        5.49 μs ± 464 ns, 1.23x
      ParStrategies IO Median3:       OK
        5.72 μs ± 171 ns, 1.28x
      Sequential ST Median3or5:       OK
        4.86 μs ± 485 ns, 1.08x
      Sequential IO Median3or5:       OK
        5.20 μs ± 350 ns, 1.16x
      ParStrategies ST Median3or5:    OK
        5.62 μs ± 442 ns, 1.25x
      ParStrategies IO Median3or5:    OK
        5.64 μs ± 473 ns, 1.26x
      Threads IO Median3:             OK
        6.15 μs ± 413 ns, 1.37x
      Threads IO Median3or5:          OK
        6.27 μs ± 217 ns, 1.40x
      fallback heapsort:              OK
        8.02 μs ± 409 ns, 1.79x
    Sorting 10 random arrays of length 256 with few duplicates
      C++:                            OK
        14.9 μs ± 1.2 μs
      Sequential ST Median3:          OK
        16.5 μs ± 1.4 μs, 1.11x
      Sequential IO Median3:          OK
        16.5 μs ± 1.6 μs, 1.11x
      ParStrategies ST Median3:       OK
        17.4 μs ± 1.4 μs, 1.17x
      ParStrategies IO Median3:       OK
        18.1 μs ± 1.7 μs, 1.22x
      Sequential ST Median3or5:       OK
        15.6 μs ± 1.4 μs, 1.05x
      Sequential IO Median3or5:       OK
        16.4 μs ± 1.5 μs, 1.10x
      ParStrategies ST Median3or5:    OK
        17.4 μs ± 1.5 μs, 1.17x
      ParStrategies IO Median3or5:    OK
        18.2 μs ± 1.6 μs, 1.22x
      Threads IO Median3:             OK
        17.6 μs ± 1.7 μs, 1.18x
      Threads IO Median3or5:          OK
        18.1 μs ± 1.4 μs, 1.21x
      fallback heapsort:              OK
        24.8 μs ± 1.5 μs, 1.67x
    Sorting 10 random arrays of length 1,000 with few duplicates
      C++:                            OK
        109  μs ± 5.4 μs
      Sequential ST Median3:          OK
        89.8 μs ± 2.7 μs, 0.83x
      Sequential IO Median3:          OK
        131  μs ± 7.5 μs, 1.21x
      ParStrategies ST Median3:       OK
        125  μs ±  11 μs, 1.15x
      ParStrategies IO Median3:       OK
        163  μs ±  15 μs, 1.50x
      Sequential ST Median3or5:       OK
        120  μs ± 2.6 μs, 1.10x
      Sequential IO Median3or5:       OK
        118  μs ± 4.2 μs, 1.09x
      ParStrategies ST Median3or5:    OK
        132  μs ± 6.4 μs, 1.22x
      ParStrategies IO Median3or5:    OK
        181  μs ±  18 μs, 1.67x
      Threads IO Median3:             OK
        126  μs ± 7.8 μs, 1.16x
      Threads IO Median3or5:          OK
        133  μs ±  11 μs, 1.23x
      fallback heapsort:              OK
        360  μs ±  26 μs, 3.31x
    Sorting 10 random arrays of length 10,000 with few duplicates
      C++:                            OK
        3.02 ms ± 170 μs
      Sequential ST Median3:          OK
        3.69 ms ± 337 μs, 1.22x
      Sequential IO Median3:          OK
        3.64 ms ± 356 μs, 1.21x
      ParStrategies ST Median3:       OK
        3.73 ms ± 336 μs, 1.23x
      ParStrategies IO Median3:       OK
        3.87 ms ± 354 μs, 1.28x
      Sequential ST Median3or5:       OK
        3.63 ms ± 350 μs, 1.20x
      Sequential IO Median3or5:       OK
        3.64 ms ± 350 μs, 1.20x
      ParStrategies ST Median3or5:    OK
        3.76 ms ± 336 μs, 1.24x
      ParStrategies IO Median3or5:    OK
        3.80 ms ± 367 μs, 1.26x
      Threads IO Median3:             OK
        3.67 ms ± 339 μs, 1.21x
      Threads IO Median3or5:          OK
        3.68 ms ± 358 μs, 1.22x
      fallback heapsort:              OK
        5.09 ms ± 391 μs, 1.68x
    Sorting 10 random arrays of length 100,000 with few duplicates
      C++:                            OK
        36.8 ms ± 1.6 ms
      Sequential ST Median3:          OK
        45.3 ms ± 3.4 ms, 1.23x
      Sequential IO Median3:          OK
        44.6 ms ± 2.8 ms, 1.21x
      ParStrategies ST Median3:       OK
        39.3 ms ± 2.6 ms, 1.07x
      ParStrategies IO Median3:       OK
        42.4 ms ± 3.8 ms, 1.15x
      Sequential ST Median3or5:       OK
        44.3 ms ± 2.7 ms, 1.20x
      Sequential IO Median3or5:       OK
        44.4 ms ± 2.8 ms, 1.21x
      ParStrategies ST Median3or5:    OK
        42.5 ms ± 3.5 ms, 1.15x
      ParStrategies IO Median3or5:    OK
        42.0 ms ± 2.7 ms, 1.14x
      Threads IO Median3:             OK
        27.6 ms ± 1.8 ms, 0.75x
      Threads IO Median3or5:          OK
        27.1 ms ± 2.1 ms, 0.74x
      fallback heapsort:              OK
        65.0 ms ± 2.9 ms, 1.77x
    Sorting 10 random arrays of length 1,000,000 with few duplicates
      C++:                            OK
        449  ms ±  35 ms
      Sequential ST Median3:          OK
        544  ms ±  11 ms, 1.21x
      Sequential IO Median3:          OK
        540  ms ±  11 ms, 1.20x
      ParStrategies ST Median3:       OK
        191  ms ±  15 ms, 0.42x
      ParStrategies IO Median3:       OK
        193  ms ±  10 ms, 0.43x
      Sequential ST Median3or5:       OK
        535  ms ±  11 ms, 1.19x
      Sequential IO Median3or5:       OK
        537  ms ± 8.4 ms, 1.20x
      ParStrategies ST Median3or5:    OK
        190  ms ±  12 ms, 0.42x
      ParStrategies IO Median3or5:    OK
        197  ms ±  20 ms, 0.44x
      Threads IO Median3:             OK
        249  ms ±  16 ms, 0.55x
      Threads IO Median3or5:          OK
        245  ms ±  13 ms, 0.55x
      fallback heapsort:              OK
        851  ms ±  30 ms, 1.90x
    Sorting 10 random arrays of length 16 with many duplicates
      C++:                            OK
        690  ns ±  22 ns
      Sequential ST Median3:          OK
        452  ns ±  33 ns, 0.66x
      Sequential IO Median3:          OK
        446  ns ±  43 ns, 0.65x
      ParStrategies ST Median3:       OK
        450  ns ±  42 ns, 0.65x
      ParStrategies IO Median3:       OK
        457  ns ±  29 ns, 0.66x
      Sequential ST Median3or5:       OK
        442  ns ±  42 ns, 0.64x
      Sequential IO Median3or5:       OK
        443  ns ±  42 ns, 0.64x
      ParStrategies ST Median3or5:    OK
        458  ns ±  45 ns, 0.66x
      ParStrategies IO Median3or5:    OK
        459  ns ±  43 ns, 0.67x
      Threads IO Median3:             OK
        1.23 μs ± 109 ns, 1.79x
      Threads IO Median3or5:          OK
        1.22 μs ±  92 ns, 1.77x
      fallback heapsort:              OK
        904  ns ±  52 ns, 1.31x
    Sorting 10 random arrays of length 17 with many duplicates
      C++:                            OK
        689  ns ±  57 ns
      Sequential ST Median3:          OK
        593  ns ±  28 ns, 0.86x
      Sequential IO Median3:          OK
        603  ns ±  41 ns, 0.87x
      ParStrategies ST Median3:       OK
        656  ns ±  48 ns, 0.95x
      ParStrategies IO Median3:       OK
        724  ns ±  59 ns, 1.05x
      Sequential ST Median3or5:       OK
        599  ns ±  53 ns, 0.87x
      Sequential IO Median3or5:       OK
        606  ns ±  51 ns, 0.88x
      ParStrategies ST Median3or5:    OK
        657  ns ±  46 ns, 0.95x
      ParStrategies IO Median3or5:    OK
        664  ns ±  44 ns, 0.96x
      Threads IO Median3:             OK
        1.40 μs ±  92 ns, 2.02x
      Threads IO Median3or5:          OK
        1.39 μs ±  85 ns, 2.02x
      fallback heapsort:              OK
        985  ns ±  51 ns, 1.43x
    Sorting 10 random arrays of length 100 with many duplicates
      C++:                            OK
        4.31 μs ± 357 ns
      Sequential ST Median3:          OK
        4.93 μs ± 361 ns, 1.15x
      Sequential IO Median3:          OK
        4.94 μs ± 390 ns, 1.15x
      ParStrategies ST Median3:       OK
        5.27 μs ± 416 ns, 1.22x
      ParStrategies IO Median3:       OK
        5.47 μs ± 363 ns, 1.27x
      Sequential ST Median3or5:       OK
        4.61 μs ± 436 ns, 1.07x
      Sequential IO Median3or5:       OK
        4.93 μs ± 419 ns, 1.15x
      ParStrategies ST Median3or5:    OK
        5.33 μs ± 329 ns, 1.24x
      ParStrategies IO Median3or5:    OK
        5.45 μs ± 352 ns, 1.27x
      Threads IO Median3:             OK
        5.79 μs ± 360 ns, 1.34x
      Threads IO Median3or5:          OK
        5.87 μs ± 384 ns, 1.36x
      fallback heapsort:              OK
        8.00 μs ± 331 ns, 1.86x
    Sorting 10 random arrays of length 256 with many duplicates
      C++:                            OK
        15.1 μs ± 1.2 μs
      Sequential ST Median3:          OK
        16.9 μs ± 1.7 μs, 1.12x
      Sequential IO Median3:          OK
        17.0 μs ± 1.5 μs, 1.13x
      ParStrategies ST Median3:       OK
        17.9 μs ± 795 ns, 1.19x
      ParStrategies IO Median3:       OK
        18.4 μs ± 1.6 μs, 1.22x
      Sequential ST Median3or5:       OK
        15.6 μs ± 1.4 μs, 1.04x
      Sequential IO Median3or5:       OK
        16.7 μs ± 1.4 μs, 1.11x
      ParStrategies ST Median3or5:    OK
        17.8 μs ± 1.7 μs, 1.18x
      ParStrategies IO Median3or5:    OK
        18.6 μs ± 1.6 μs, 1.24x
      Threads IO Median3:             OK
        17.9 μs ± 670 ns, 1.19x
      Threads IO Median3or5:          OK
        18.7 μs ± 1.6 μs, 1.24x
      fallback heapsort:              OK
        24.9 μs ± 1.3 μs, 1.65x
    Sorting 10 random arrays of length 1,000 with many duplicates
      C++:                            OK
        60.7 μs ± 1.3 μs
      Sequential ST Median3:          OK
        162  μs ± 3.1 μs, 2.67x
      Sequential IO Median3:          OK
        130  μs ± 4.7 μs, 2.13x
      ParStrategies ST Median3:       OK
        179  μs ± 7.1 μs, 2.94x
      ParStrategies IO Median3:       OK
        159  μs ± 3.8 μs, 2.62x
      Sequential ST Median3or5:       OK
        126  μs ±  12 μs, 2.07x
      Sequential IO Median3or5:       OK
        139  μs ± 7.3 μs, 2.29x
      ParStrategies ST Median3or5:    OK
        127  μs ± 8.9 μs, 2.09x
      ParStrategies IO Median3or5:    OK
        151  μs ± 6.1 μs, 2.49x
      Threads IO Median3:             OK
        86.0 μs ± 5.6 μs, 1.42x
      Threads IO Median3or5:          OK
        83.4 μs ± 7.7 μs, 1.37x
      fallback heapsort:              OK
        352  μs ±  25 μs, 5.80x
    Sorting 10 random arrays of length 10,000 with many duplicates
      C++:                            OK
        2.69 ms ± 173 μs
      Sequential ST Median3:          OK
        3.09 ms ± 276 μs, 1.15x
      Sequential IO Median3:          OK
        2.98 ms ± 190 μs, 1.11x
      ParStrategies ST Median3:       OK
        3.06 ms ± 212 μs, 1.14x
      ParStrategies IO Median3:       OK
        3.19 ms ± 186 μs, 1.19x
      Sequential ST Median3or5:       OK
        3.02 ms ± 191 μs, 1.12x
      Sequential IO Median3or5:       OK
        3.01 ms ± 186 μs, 1.12x
      ParStrategies ST Median3or5:    OK
        3.11 ms ± 187 μs, 1.16x
      ParStrategies IO Median3or5:    OK
        3.14 ms ± 192 μs, 1.17x
      Threads IO Median3:             OK
        3.05 ms ± 171 μs, 1.14x
      Threads IO Median3or5:          OK
        3.08 ms ± 270 μs, 1.15x
      fallback heapsort:              OK
        4.97 ms ± 337 μs, 1.85x
    Sorting 10 random arrays of length 100,000 with many duplicates
      C++:                            OK
        26.6 ms ± 1.4 ms
      Sequential ST Median3:          OK
        27.7 ms ± 1.3 ms, 1.04x
      Sequential IO Median3:          OK
        27.2 ms ± 1.4 ms, 1.02x
      ParStrategies ST Median3:       OK
        26.0 ms ± 1.1 ms, 0.98x
      ParStrategies IO Median3:       OK
        27.2 ms ± 2.5 ms, 1.02x
      Sequential ST Median3or5:       OK
        27.1 ms ± 1.6 ms, 1.02x
      Sequential IO Median3or5:       OK
        27.6 ms ± 1.9 ms, 1.04x
      ParStrategies ST Median3or5:    OK
        26.7 ms ± 2.1 ms, 1.00x
      ParStrategies IO Median3or5:    OK
        26.2 ms ± 2.1 ms, 0.98x
      Threads IO Median3:             OK
        18.2 ms ± 900 μs, 0.68x
      Threads IO Median3or5:          OK
        17.9 ms ± 1.3 ms, 0.67x
      fallback heapsort:              OK
        64.5 ms ± 2.8 ms, 2.42x
    Sorting 10 random arrays of length 1,000,000 with many duplicates
      C++:                            OK
        281  ms ±  25 ms
      Sequential ST Median3:          OK
        274  ms ± 2.9 ms, 0.98x
      Sequential IO Median3:          OK
        269  ms ± 5.3 ms, 0.96x
      ParStrategies ST Median3:       OK
        189  ms ± 8.9 ms, 0.67x
      ParStrategies IO Median3:       OK
        194  ms ±  11 ms, 0.69x
      Sequential ST Median3or5:       OK
        263  ms ± 3.2 ms, 0.94x
      Sequential IO Median3or5:       OK
        266  ms ± 2.9 ms, 0.95x
      ParStrategies ST Median3or5:    OK
        188  ms ±  14 ms, 0.67x
      ParStrategies IO Median3or5:    OK
        194  ms ±  11 ms, 0.69x
      Threads IO Median3:             OK
        151  ms ± 7.5 ms, 0.54x
      Threads IO Median3or5:          OK
        147  ms ±  12 ms, 0.53x
      fallback heapsort:              OK
        784  ms ±  11 ms, 2.79x
  (Double, Double, Int64)
    Sorting 10 random arrays of length 16 with few duplicates
      Sanity:                         OK
      C++ single vector:              OK
        841  ns ±  57 ns
      C++ three vectors:              OK
        2.35 μs ± 176 ns
      Sequential ST Median3 Unbox:    OK
        1.34 μs ± 128 ns
      Sequential ST Median3or5 Unbox: OK
        1.36 μs ±  91 ns
      Sequential ST Median3 Prim:     OK
        1.23 μs ± 103 ns
      Sequential ST Median3or5 Prim:  OK
        1.22 μs ± 112 ns
    Sorting 10 random arrays of length 17 with few duplicates
      Sanity:                         OK
      C++ single vector:              OK
        901  ns ±  26 ns
      C++ three vectors:              OK
        2.60 μs ± 207 ns
      Sequential ST Median3 Unbox:    OK
        1.59 μs ± 126 ns
      Sequential ST Median3or5 Unbox: OK
        1.70 μs ± 167 ns
      Sequential ST Median3 Prim:     OK
        1.44 μs ± 103 ns
      Sequential ST Median3or5 Prim:  OK
        1.46 μs ± 113 ns
    Sorting 10 random arrays of length 100 with few duplicates
      Sanity:                         OK
      C++ single vector:              OK
        7.72 μs ± 581 ns
      C++ three vectors:              OK
        14.5 μs ± 1.1 μs
      Sequential ST Median3 Unbox:    OK
        10.4 μs ± 959 ns
      Sequential ST Median3or5 Unbox: OK
        10.2 μs ± 745 ns
      Sequential ST Median3 Prim:     OK
        12.3 μs ± 809 ns
      Sequential ST Median3or5 Prim:  OK
        12.5 μs ± 795 ns
    Sorting 10 random arrays of length 256 with few duplicates
      Sanity:                         OK
      C++ single vector:              OK
        14.6 μs ± 682 ns
      C++ three vectors:              OK
        42.7 μs ± 4.2 μs
      Sequential ST Median3 Unbox:    OK
        34.3 μs ± 1.7 μs
      Sequential ST Median3or5 Unbox: OK
        33.5 μs ± 1.4 μs
      Sequential ST Median3 Prim:     OK
        30.7 μs ± 2.6 μs
      Sequential ST Median3or5 Prim:  OK
        32.0 μs ± 2.1 μs
    Sorting 10 random arrays of length 1,000 with few duplicates
      Sanity:                         OK
      C++ single vector:              OK
        69.4 μs ± 4.4 μs
      C++ three vectors:              OK
        212  μs ±  16 μs
      Sequential ST Median3 Unbox:    OK
        265  μs ±  23 μs
      Sequential ST Median3or5 Unbox: OK
        294  μs ±  26 μs
      Sequential ST Median3 Prim:     OK
        299  μs ±  27 μs
      Sequential ST Median3or5 Prim:  OK
        305  μs ±  23 μs
    Sorting 10 random arrays of length 10,000 with few duplicates
      Sanity:                         OK
      C++ single vector:              OK
        3.27 ms ± 195 μs
      C++ three vectors:              OK
        3.62 ms ± 352 μs
      Sequential ST Median3 Unbox:    OK
        5.11 ms ± 338 μs
      Sequential ST Median3or5 Unbox: OK
        5.17 ms ± 398 μs
      Sequential ST Median3 Prim:     OK
        5.70 ms ± 369 μs
      Sequential ST Median3or5 Prim:  OK
        5.64 ms ± 370 μs
    Sorting 10 random arrays of length 100,000 with few duplicates
      Sanity:                         OK
      C++ single vector:              OK
        40.7 ms ± 1.6 ms
      C++ three vectors:              OK
        43.8 ms ± 1.4 ms
      Sequential ST Median3 Unbox:    OK
        62.2 ms ± 2.7 ms
      Sequential ST Median3or5 Unbox: OK
        62.9 ms ± 3.0 ms
      Sequential ST Median3 Prim:     OK
        70.7 ms ± 1.8 ms
      Sequential ST Median3or5 Prim:  OK
        69.7 ms ± 3.4 ms
    Sorting 10 random arrays of length 1,000,000 with few duplicates
      Sanity:                         OK
      C++ single vector:              OK
        510  ms ± 3.0 ms
      C++ three vectors:              OK
        542  ms ±  18 ms
      Sequential ST Median3 Unbox:    OK
        752  ms ±  14 ms
      Sequential ST Median3or5 Unbox: OK
        757  ms ±  35 ms
      Sequential ST Median3 Prim:     OK
        848  ms ±  73 ms
      Sequential ST Median3or5 Prim:  OK
        823  ms ±  20 ms
    Sorting 10 random arrays of length 16 with many duplicates
      Sanity:                         OK
      C++ single vector:              OK
        876  ns ±  54 ns
      C++ three vectors:              OK
        2.40 μs ± 181 ns
      Sequential ST Median3 Unbox:    OK
        1.45 μs ± 132 ns
      Sequential ST Median3or5 Unbox: OK
        1.44 μs ± 115 ns
      Sequential ST Median3 Prim:     OK
        1.39 μs ± 102 ns
      Sequential ST Median3or5 Prim:  OK
        1.41 μs ±  94 ns
    Sorting 10 random arrays of length 17 with many duplicates
      Sanity:                         OK
      C++ single vector:              OK
        889  ns ±  57 ns
      C++ three vectors:              OK
        2.63 μs ± 181 ns
      Sequential ST Median3 Unbox:    OK
        1.64 μs ± 120 ns
      Sequential ST Median3or5 Unbox: OK
        1.65 μs ± 130 ns
      Sequential ST Median3 Prim:     OK
        1.47 μs ± 107 ns
      Sequential ST Median3or5 Prim:  OK
        1.47 μs ± 119 ns
    Sorting 10 random arrays of length 100 with many duplicates
      Sanity:                         OK
      C++ single vector:              OK
        7.62 μs ± 597 ns
      C++ three vectors:              OK
        13.8 μs ± 693 ns
      Sequential ST Median3 Unbox:    OK
        10.8 μs ± 790 ns
      Sequential ST Median3or5 Unbox: OK
        10.5 μs ± 839 ns
      Sequential ST Median3 Prim:     OK
        12.7 μs ± 814 ns
      Sequential ST Median3or5 Prim:  OK
        12.9 μs ± 910 ns
    Sorting 10 random arrays of length 256 with many duplicates
      Sanity:                         OK
      C++ single vector:              OK
        14.8 μs ± 978 ns
      C++ three vectors:              OK
        38.7 μs ± 2.7 μs
      Sequential ST Median3 Unbox:    OK
        34.0 μs ± 3.4 μs
      Sequential ST Median3or5 Unbox: OK
        34.0 μs ± 3.0 μs
      Sequential ST Median3 Prim:     OK
        31.6 μs ± 3.1 μs
      Sequential ST Median3or5 Prim:  OK
        32.5 μs ± 2.3 μs
    Sorting 10 random arrays of length 1,000 with many duplicates
      Sanity:                         OK
      C++ single vector:              OK
        69.2 μs ± 4.9 μs
      C++ three vectors:              OK
        151  μs ±  11 μs
      Sequential ST Median3 Unbox:    OK
        256  μs ±  17 μs
      Sequential ST Median3or5 Unbox: OK
        291  μs ±  15 μs
      Sequential ST Median3 Prim:     OK
        303  μs ±  26 μs
      Sequential ST Median3or5 Prim:  OK
        292  μs ± 7.8 μs
    Sorting 10 random arrays of length 10,000 with many duplicates
      Sanity:                         OK
      C++ single vector:              OK
        2.98 ms ± 193 μs
      C++ three vectors:              OK
        3.48 ms ± 182 μs
      Sequential ST Median3 Unbox:    OK
        4.30 ms ± 345 μs
      Sequential ST Median3or5 Unbox: OK
        4.50 ms ± 446 μs
      Sequential ST Median3 Prim:     OK
        4.80 ms ± 339 μs
      Sequential ST Median3or5 Prim:  OK
        4.87 ms ± 407 μs
    Sorting 10 random arrays of length 100,000 with many duplicates
      Sanity:                         OK
      C++ single vector:              OK
        30.2 ms ± 1.8 ms
      C++ three vectors:              OK
        41.1 ms ± 1.8 ms
      Sequential ST Median3 Unbox:    OK
        40.8 ms ± 1.9 ms
      Sequential ST Median3or5 Unbox: OK
        41.6 ms ± 1.7 ms
      Sequential ST Median3 Prim:     OK
        43.7 ms ± 1.5 ms
      Sequential ST Median3or5 Prim:  OK
        43.6 ms ± 3.0 ms
    Sorting 10 random arrays of length 1,000,000 with many duplicates
      Sanity:                         OK
      C++ single vector:              OK
        337  ms ±  11 ms
      C++ three vectors:              OK
        420  ms ± 5.0 ms
      Sequential ST Median3 Unbox:    OK
        389  ms ± 3.3 ms
      Sequential ST Median3or5 Unbox: OK
        398  ms ± 3.4 ms
      Sequential ST Median3 Prim:     OK
        452  ms ±  27 ms
      Sequential ST Median3or5 Prim:  OK
        428  ms ± 2.9 ms

```


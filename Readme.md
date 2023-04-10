[![build](https://github.com/sergv/atomic-counter/actions/workflows/haskell-ci.yaml/badge.svg)](https://github.com/sergv/atomic-counter/actions/workflows/haskell-ci.yaml)

# Synopsis

This package features reasonable sort function that is a good default
for sorting mutable vectors using well-known quicksort algorithm.
During development one of the goals was making it perform on par with
C++’s `std::sort`, i.e. fast.

While providing reasonably good single-threaded default sorting, the
algorithm in this package is also parallelisable and can provides ways
to run on multiple cores using either threads or sparks, so bigger
vectors can be sorted even faster.

# Algorithm details

Technically it’s an (introsort)[https://en.wikipedia.org/wiki/Introsort], i.e.
pathological `O(N^2)` quicksort case will get delegated to heapsort when recursion
gets too deep, thus making algorithm reliably `O(N * log(N))`.

# Benchmarks

The numbers in the `Overhead thaw` benchmark should be substracted
from all the subsequent benchmarks to obtain how long the actual sort
took.

Note: the `Sorting fuzzy matching scores vector` is `test.txt` input
in this repository, it’s a reasonably big vector (around 40000 items)
with lots of duplicates that I managed to come by in the real world.

The `Sequential ST Median3or5` is the default sort exported in
`Data.Vector.Algorithms.Quicksort`.

```
$ cabal run bench -- -j1 --time-mode wall --timeout 60
All
  Overhead thaw
    fuzzy scores vector:           OK (20.84s)
      19.5 μs ± 152 ns
    1000 vectors of length 16:     OK (15.39s)
      26.9 μs ± 1.8 μs
    1000 vectors of length 17:     OK (14.19s)
      28.8 μs ± 1.3 μs
    1000 vectors of length 100:    OK (14.39s)
      92.8 μs ± 5.1 μs
    1000 vectors of length 256:    OK (10.62s)
      278  μs ±  19 μs
    1000 vectors of length 20000:  OK (4.37s)
      16.8 ms ± 1.4 ms
    1000 vectors of length 39867:  OK (13.23s)
      37.6 ms ± 2.3 ms
  Sorting fuzzy matching scores vector
    C++:                           OK (0.25s)
      374  μs ±  33 μs
    Sequential ST Median3:         OK (0.19s)
      336  μs ±  22 μs, 0.90x
    Sequential IO Median3:         OK (0.20s)
      357  μs ±  22 μs, 0.95x
    ParStrategies ST Median3:      OK (0.31s)
      136  μs ± 7.0 μs, 0.36x
    ParStrategies IO Median3:      OK (0.21s)
      83.8 μs ± 7.4 μs, 0.22x
    Sequential ST Median3or5:      OK (0.32s)
      294  μs ±  18 μs, 0.79x
    Sequential IO Median3or5:      OK (0.33s)
      296  μs ±  16 μs, 0.79x
    ParStrategies ST Median3or5:   OK (0.73s)
      80.9 μs ± 5.3 μs, 0.22x
    ParStrategies IO Median3or5:   OK (0.20s)
      79.4 μs ± 7.3 μs, 0.21x
    Sequential ST AveragingMedian: OK (0.14s)
      2.04 ms ± 172 μs, 5.46x
    Threads IO Median3:            OK (1.52s)
      355  μs ±  27 μs, 0.95x
    Threads IO Median3or5:         OK (0.15s)
      263  μs ±  22 μs, 0.70x
    vector-algorithms heapsort:    OK (0.15s)
      2.12 ms ± 169 μs, 5.66x
    fallback heapsort:             OK (0.19s)
      2.68 ms ± 208 μs, 7.16x
  Sorting 1000 random arrays of length 16
    C++:                           OK (0.39s)
      174  μs ± 8.6 μs
    Sequential ST Median3:         OK (0.23s)
      199  μs ±  11 μs, 1.14x
    Sequential IO Median3:         OK (0.23s)
      203  μs ±  17 μs, 1.17x
    ParStrategies ST Median3:      OK (0.43s)
      195  μs ± 6.9 μs, 1.12x
    ParStrategies IO Median3:      OK (0.22s)
      193  μs ±  11 μs, 1.11x
    Sequential ST Median3or5:      OK (0.22s)
      193  μs ±  19 μs, 1.11x
    Sequential IO Median3or5:      OK (0.23s)
      198  μs ±  14 μs, 1.14x
    ParStrategies ST Median3or5:   OK (0.23s)
      199  μs ±  15 μs, 1.14x
    ParStrategies IO Median3or5:   OK (0.22s)
      198  μs ±  12 μs, 1.14x
    Sequential ST AveragingMedian: OK (0.22s)
      194  μs ±  12 μs, 1.11x
    Threads IO Median3:            OK (0.17s)
      300  μs ±  27 μs, 1.73x
    Threads IO Median3or5:         OK (0.24s)
      422  μs ±  40 μs, 2.43x
    vector-algorithms heapsort:    OK (0.26s)
      455  μs ±  41 μs, 2.62x
    fallback heapsort:             OK (0.24s)
      399  μs ±  22 μs, 2.29x
  Sorting 1000 random arrays of length 17
    C++:                           OK (0.19s)
      313  μs ±  24 μs
    Sequential ST Median3:         OK (0.22s)
      368  μs ±  25 μs, 1.18x
    Sequential IO Median3:         OK (0.59s)
      272  μs ±  17 μs, 0.87x
    ParStrategies ST Median3:      OK (0.17s)
      280  μs ±  27 μs, 0.89x
    ParStrategies IO Median3:      OK (0.22s)
      365  μs ±  25 μs, 1.17x
    Sequential ST Median3or5:      OK (0.22s)
      364  μs ±  21 μs, 1.16x
    Sequential IO Median3or5:      OK (0.31s)
      279  μs ±  20 μs, 0.89x
    ParStrategies ST Median3or5:   OK (0.17s)
      280  μs ±  26 μs, 0.89x
    ParStrategies IO Median3or5:   OK (0.23s)
      387  μs ±  38 μs, 1.24x
    Sequential ST AveragingMedian: OK (0.31s)
      551  μs ±  44 μs, 1.76x
    Threads IO Median3:            OK (0.21s)
      365  μs ±  23 μs, 1.16x
    Threads IO Median3or5:         OK (0.15s)
      500  μs ±  44 μs, 1.60x
    vector-algorithms heapsort:    OK (0.21s)
      358  μs ±  29 μs, 1.14x
    fallback heapsort:             OK (0.26s)
      443  μs ±  30 μs, 1.42x
  Sorting 1000 random arrays of length 100
    C++:                           OK (0.27s)
      2.02 ms ± 171 μs
    Sequential ST Median3:         OK (0.25s)
      3.57 ms ± 229 μs, 1.77x
    Sequential IO Median3:         OK (0.13s)
      3.58 ms ± 353 μs, 1.77x
    ParStrategies ST Median3:      OK (0.26s)
      3.79 ms ± 238 μs, 1.88x
    ParStrategies IO Median3:      OK (0.24s)
      3.51 ms ± 193 μs, 1.74x
    Sequential ST Median3or5:      OK (0.18s)
      2.58 ms ± 172 μs, 1.28x
    Sequential IO Median3or5:      OK (0.24s)
      3.45 ms ± 253 μs, 1.71x
    ParStrategies ST Median3or5:   OK (0.19s)
      2.77 ms ± 253 μs, 1.37x
    ParStrategies IO Median3or5:   OK (0.25s)
      3.64 ms ± 215 μs, 1.80x
    Sequential ST AveragingMedian: OK (0.13s)
      3.83 ms ± 339 μs, 1.90x
    Threads IO Median3:            OK (0.26s)
      3.70 ms ± 192 μs, 1.84x
    Threads IO Median3or5:         OK (0.19s)
      2.71 ms ± 259 μs, 1.34x
    vector-algorithms heapsort:    OK (0.18s)
      5.18 ms ± 420 μs, 2.57x
    fallback heapsort:             OK (0.17s)
      4.77 ms ± 356 μs, 2.37x
  Sorting 1000 random arrays of length 256
    C++:                           OK (0.14s)
      8.33 ms ± 719 μs
    Sequential ST Median3:         OK (0.24s)
      7.39 ms ± 638 μs, 0.89x
    Sequential IO Median3:         OK (0.17s)
      10.6 ms ± 791 μs, 1.27x
    ParStrategies ST Median3:      OK (0.24s)
      7.12 ms ± 522 μs, 0.85x
    ParStrategies IO Median3:      OK (0.22s)
      6.52 ms ± 651 μs, 0.78x
    Sequential ST Median3or5:      OK (0.17s)
      10.3 ms ± 939 μs, 1.24x
    Sequential IO Median3or5:      OK (0.16s)
      9.92 ms ± 685 μs, 1.19x
    ParStrategies ST Median3or5:   OK (0.16s)
      9.89 ms ± 896 μs, 1.19x
    ParStrategies IO Median3or5:   OK (0.47s)
      6.91 ms ± 300 μs, 0.83x
    Sequential ST AveragingMedian: OK (0.18s)
      11.3 ms ± 794 μs, 1.36x
    Threads IO Median3:            OK (0.14s)
      8.62 ms ± 737 μs, 1.03x
    Threads IO Median3or5:         OK (1.52s)
      11.7 ms ± 697 μs, 1.41x
    vector-algorithms heapsort:    OK (0.19s)
      12.0 ms ± 1.1 ms, 1.44x
    fallback heapsort:             OK (0.17s)
      10.6 ms ± 943 μs, 1.27x
  Sorting 1000 random arrays of length 20000
    C++:                           OK (1.54s)
      508  ms ±  45 ms
    Sequential ST Median3:         OK (1.73s)
      572  ms ±  17 ms, 1.13x
    Sequential IO Median3:         OK (1.96s)
      651  ms ± 4.3 ms, 1.28x
    ParStrategies ST Median3:      OK (0.44s)
      143  ms ± 6.2 ms, 0.28x
    ParStrategies IO Median3:      OK (2.31s)
      154  ms ±  11 ms, 0.30x
    Sequential ST Median3or5:      OK (1.69s)
      562  ms ± 5.4 ms, 1.11x
    Sequential IO Median3or5:      OK (1.68s)
      556  ms ±  10 ms, 1.10x
    ParStrategies ST Median3or5:   OK (21.11s)
      168  ms ± 6.5 ms, 0.33x
    ParStrategies IO Median3or5:   OK (0.44s)
      143  ms ± 3.8 ms, 0.28x
    Sequential ST AveragingMedian: OK (4.24s)
      1.410 s ±  36 ms, 2.78x
    Threads IO Median3:            OK (1.56s)
      514  ms ± 5.3 ms, 1.01x
    Threads IO Median3or5:         OK (1.16s)
      383  ms ±  13 ms, 0.75x
    vector-algorithms heapsort:    OK (4.86s)
      1.620 s ±  41 ms, 3.19x
    fallback heapsort:             OK (4.18s)
      1.390 s ±  11 ms, 2.74x
  Sorting 1000 random arrays of length 39867
    C++:                           OK (7.48s)
      1.053 s ±  12 ms
    Sequential ST Median3:         OK (3.46s)
      1.148 s ±  69 ms, 1.09x
    Sequential IO Median3:         OK (3.91s)
      1.303 s ±  74 ms, 1.24x
    ParStrategies ST Median3:      OK (18.73s)
      287  ms ± 490 μs, 0.27x
    ParStrategies IO Median3:      OK (0.73s)
      240  ms ± 3.5 ms, 0.23x
    Sequential ST Median3or5:      OK (3.29s)
      1.090 s ±  21 ms, 1.04x
    Sequential IO Median3or5:      OK (3.29s)
      1.093 s ± 4.2 ms, 1.04x
    ParStrategies ST Median3or5:   OK (0.85s)
      279  ms ± 5.5 ms, 0.27x
    ParStrategies IO Median3or5:   OK (0.86s)
      285  ms ±  14 ms, 0.27x
    Sequential ST AveragingMedian: OK (8.86s)
      2.948 s ± 6.4 ms, 2.80x
    Threads IO Median3:            OK (2.90s)
      968  ms ±  93 ms, 0.92x
    Threads IO Median3or5:         OK (5.42s)
      782  ms ±  58 ms, 0.74x
    vector-algorithms heapsort:    OK (10.45s)
      3.476 s ±  69 ms, 3.30x
    fallback heapsort:             OK (8.84s)
      2.944 s ±  54 ms, 2.80x
```


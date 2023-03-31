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

Major difference to vanilla quicksort is

# Is it fast?

Provisional benchmark results

Note: the `Sorting fuzzy matching scores vector` is `test.txt` input
in this repository, it’s a reasonably big vector (around 40000 items)
with lots of duplicates.

```
All
  Overhead thaw
    fuzzy scores vector:          OK (14.87s)
      17.9 μs ± 1.6 μs
    1000 vectors of length 16:    OK (12.80s)
      25.8 μs ± 2.5 μs
    1000 vectors of length 17:    OK (13.32s)
      29.2 μs ± 1.4 μs
    1000 vectors of length 100:   OK (13.02s)
      83.9 μs ± 3.5 μs
    1000 vectors of length 256:   OK (10.03s)
      269  μs ±  24 μs
    1000 vectors of length 20000: OK (3.48s)
      16.5 ms ± 1.4 ms
    1000 vectors of length 39867: OK (8.36s)
      37.8 ms ± 2.7 ms
  Sorting fuzzy matching scores vector
    C++:                          OK (0.23s)
      365  μs ±  32 μs
    Sequential ST Median3:        OK (0.18s)
      303  μs ±  27 μs, 0.83x
    Sequential IO Median3:        OK (0.60s)
      280  μs ±  15 μs, 0.77x
    ParStrategies IO Median3:     OK (0.21s)
      89.1 μs ± 6.2 μs, 0.24x
    ParStrategies ST Median3:     OK (0.22s)
      92.1 μs ± 7.4 μs, 0.25x
    Sequential ST Median3or5:     OK (0.25s)
      225  μs ±  20 μs, 0.62x
    Sequential IO Median3or5:     OK (0.22s)
      196  μs ±  12 μs, 0.54x
    ParStrategies IO Median3or5:  OK (0.31s)
      65.9 μs ± 3.8 μs, 0.18x
    ParStrategies ST Median3or5:  OK (0.34s)
      75.8 μs ± 4.2 μs, 0.21x
    vector-algorithms heapsort:   OK (0.28s)
      2.07 ms ±  95 μs, 5.68x
    fallback heapsort:            OK (0.13s)
      1.80 ms ± 169 μs, 4.94x
  Sorting 1000 random arrays of length 16
    C++:                          OK (0.18s)
      158  μs ±  15 μs
    Sequential ST Median3:        OK (0.41s)
      193  μs ±  12 μs, 1.22x
    Sequential IO Median3:        OK (0.22s)
      197  μs ±  11 μs, 1.24x
    ParStrategies IO Median3:     OK (0.40s)
      186  μs ± 5.5 μs, 1.17x
    ParStrategies ST Median3:     OK (0.21s)
      191  μs ±  13 μs, 1.21x
    Sequential ST Median3or5:     OK (0.21s)
      188  μs ±  18 μs, 1.19x
    Sequential IO Median3or5:     OK (0.40s)
      185  μs ± 7.5 μs, 1.17x
    ParStrategies IO Median3or5:  OK (0.21s)
      187  μs ±  11 μs, 1.18x
    ParStrategies ST Median3or5:  OK (0.22s)
      194  μs ±  11 μs, 1.23x
    vector-algorithms heapsort:   OK (0.19s)
      331  μs ±  23 μs, 2.09x
    fallback heapsort:            OK (0.31s)
      280  μs ±  19 μs, 1.77x
  Sorting 1000 random arrays of length 17
    C++:                          OK (0.24s)
      211  μs ±  15 μs
    Sequential ST Median3:        OK (0.14s)
      240  μs ±  24 μs, 1.14x
    Sequential IO Median3:        OK (0.13s)
      230  μs ±  22 μs, 1.09x
    ParStrategies IO Median3:     OK (0.27s)
      253  μs ±  25 μs, 1.20x
    ParStrategies ST Median3:     OK (0.27s)
      248  μs ±  11 μs, 1.18x
    Sequential ST Median3or5:     OK (0.28s)
      248  μs ±  18 μs, 1.18x
    Sequential IO Median3or5:     OK (0.14s)
      239  μs ±  23 μs, 1.13x
    ParStrategies IO Median3or5:  OK (0.14s)
      249  μs ±  23 μs, 1.18x
    ParStrategies ST Median3or5:  OK (0.53s)
      247  μs ± 5.5 μs, 1.17x
    vector-algorithms heapsort:   OK (0.20s)
      348  μs ±  21 μs, 1.65x
    fallback heapsort:            OK (0.18s)
      311  μs ±  26 μs, 1.47x
  Sorting 1000 random arrays of length 100
    C++:                          OK (0.26s)
      1.97 ms ± 171 μs
    Sequential ST Median3:        OK (0.30s)
      2.21 ms ± 141 μs, 1.12x
    Sequential IO Median3:        OK (0.28s)
      2.13 ms ± 121 μs, 1.08x
    ParStrategies IO Median3:     OK (0.16s)
      2.31 ms ± 186 μs, 1.17x
    ParStrategies ST Median3:     OK (0.31s)
      2.36 ms ± 137 μs, 1.19x
    Sequential ST Median3or5:     OK (0.16s)
      2.26 ms ± 175 μs, 1.14x
    Sequential IO Median3or5:     OK (0.29s)
      2.20 ms ± 125 μs, 1.11x
    ParStrategies IO Median3or5:  OK (0.16s)
      2.38 ms ± 171 μs, 1.21x
    ParStrategies ST Median3or5:  OK (0.17s)
      2.46 ms ± 184 μs, 1.25x
    vector-algorithms heapsort:   OK (0.25s)
      3.75 ms ± 280 μs, 1.90x
    fallback heapsort:            OK (0.41s)
      3.05 ms ± 110 μs, 1.55x
  Sorting 1000 random arrays of length 256
    C++:                          OK (0.38s)
      5.77 ms ± 244 μs
    Sequential ST Median3:        OK (0.22s)
      6.75 ms ± 646 μs, 1.17x
    Sequential IO Median3:        OK (0.21s)
      6.57 ms ± 378 μs, 1.14x
    ParStrategies IO Median3:     OK (0.21s)
      6.34 ms ± 386 μs, 1.10x
    ParStrategies ST Median3:     OK (0.20s)
      6.21 ms ± 367 μs, 1.08x
    Sequential ST Median3or5:     OK (0.22s)
      6.79 ms ± 615 μs, 1.18x
    Sequential IO Median3or5:     OK (0.43s)
      6.64 ms ± 451 μs, 1.15x
    ParStrategies IO Median3or5:  OK (0.20s)
      6.20 ms ± 363 μs, 1.08x
    ParStrategies ST Median3or5:  OK (0.21s)
      6.48 ms ± 459 μs, 1.12x
    vector-algorithms heapsort:   OK (0.19s)
      12.0 ms ± 1.2 ms, 2.08x
    fallback heapsort:            OK (0.15s)
      9.46 ms ± 868 μs, 1.64x
  Sorting 1000 random arrays of length 20000
    C++:                          OK (1.50s)
      500  ms ± 6.1 ms
    Sequential ST Median3:        OK (1.71s)
      567  ms ± 6.1 ms, 1.14x
    Sequential IO Median3:        OK (1.57s)
      521  ms ± 3.1 ms, 1.04x
    ParStrategies IO Median3:     OK (2.53s)
      166  ms ± 7.7 ms, 0.33x
    ParStrategies ST Median3:     OK (0.40s)
      131  ms ± 7.0 ms, 0.26x
    Sequential ST Median3or5:     OK (1.54s)
      508  ms ±  22 ms, 1.02x
    Sequential IO Median3or5:     OK (1.54s)
      510  ms ±  12 ms, 1.02x
    ParStrategies IO Median3or5:  OK (0.39s)
      127  ms ± 2.9 ms, 0.25x
    ParStrategies ST Median3or5:  OK (1.01s)
      144  ms ± 8.8 ms, 0.29x
    vector-algorithms heapsort:   OK (4.84s)
      1.608 s ±  11 ms, 3.22x
    fallback heapsort:            OK (3.78s)
      1.255 s ±  62 ms, 2.51x
  Sorting 1000 random arrays of length 39867
    C++:                          OK (7.36s)
      1.032 s ± 4.7 ms
    Sequential ST Median3:        OK (3.47s)
      1.151 s ±  70 ms, 1.12x
    Sequential IO Median3:        OK (3.22s)
      1.075 s ± 101 ms, 1.04x
    ParStrategies IO Median3:     OK (18.14s)
      275  ms ±  12 ms, 0.27x
    ParStrategies ST Median3:     OK (0.78s)
      254  ms ± 2.9 ms, 0.25x
    Sequential ST Median3or5:     OK (3.01s)
      0.999 s ±  16 ms, 0.97x
    Sequential IO Median3or5:     OK (2.95s)
      980  ms ± 5.2 ms, 0.95x
    ParStrategies IO Median3or5:  OK (0.75s)
      246  ms ± 3.5 ms, 0.24x
    ParStrategies ST Median3or5:  OK (4.70s)
      321  ms ±  11 ms, 0.31x
    vector-algorithms heapsort:   OK (10.36s)
      3.440 s ±  11 ms, 3.33x
    fallback heapsort:            OK (7.92s)
      2.644 s ± 101 ms, 2.56x
```


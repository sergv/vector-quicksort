[![build](https://github.com/sergv/vecttor-quicksort/actions/workflows/haskell-ci.yaml/badge.svg)](https://github.com/sergv/vecttor-quicksort/actions/workflows/haskell-ci.yaml)

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

Test setup: Intel i5-9600K CPU @ 3.70GHz, 6 cores, no hyperthreading, GHC 9.6.1.

The default sort exported in `Data.Vector.Algorithms.Quicksort` is `Sequential` and `Median3or5`.

```
$ cabal run bench -- -j1 --time-mode wall --timeout 300
All
  Sorting fuzzy matching scores vector
    C++:                           OK (0.54s)
      377  μs ±  30 μs
    Sequential ST Median3:         OK (0.36s)
      315  μs ±  17 μs, 0.83x
    Sequential IO Median3:         OK (0.31s)
      288  μs ±  17 μs, 0.76x
    ParStrategies ST Median3:      OK (0.15s)
      285  μs ±  23 μs, 0.76x
    ParStrategies IO Median3:      OK (0.16s)
      306  μs ±  22 μs, 0.81x
    Sequential ST Median3or5:      OK (0.28s)
      261  μs ±  13 μs, 0.69x
    Sequential IO Median3or5:      OK (0.28s)
      264  μs ±  25 μs, 0.70x
    ParStrategies ST Median3or5:   OK (0.14s)
      268  μs ±  21 μs, 0.71x
    ParStrategies IO Median3or5:   OK (0.15s)
      272  μs ±  27 μs, 0.72x
    Sequential ST AveragingMedian: OK (0.16s)
      2.47 ms ± 191 μs, 6.55x
    Threads IO Median3:            OK (0.31s)
      295  μs ±  28 μs, 0.78x
    Threads IO Median3or5:         OK (0.24s)
      230  μs ±  12 μs, 0.61x
    vector-algorithms heapsort:    OK (0.27s)
      2.06 ms ±  97 μs, 5.45x
    fallback heapsort:             OK (0.25s)
      1.91 ms ± 106 μs, 5.07x
  Sorting 10 random arrays of length 16 with few duplicates
    C++:                           OK (0.36s)
      654  ns ±  24 ns
    Sequential ST Median3:         OK (0.31s)
      554  ns ±  23 ns, 0.85x
    Sequential IO Median3:         OK (0.16s)
      559  ns ±  44 ns, 0.85x
    ParStrategies ST Median3:      OK (0.16s)
      578  ns ±  58 ns, 0.88x
    ParStrategies IO Median3:      OK (0.32s)
      578  ns ±  26 ns, 0.88x
    Sequential ST Median3or5:      OK (0.16s)
      569  ns ±  50 ns, 0.87x
    Sequential IO Median3or5:      OK (0.16s)
      571  ns ±  50 ns, 0.87x
    ParStrategies ST Median3or5:   OK (0.17s)
      580  ns ±  42 ns, 0.89x
    ParStrategies IO Median3or5:   OK (0.16s)
      577  ns ±  49 ns, 0.88x
    Sequential ST AveragingMedian: OK (0.31s)
      557  ns ±  27 ns, 0.85x
    Threads IO Median3:            OK (0.24s)
      1.71 μs ± 128 ns, 2.62x
    Threads IO Median3or5:         OK (0.24s)
      1.73 μs ± 120 ns, 2.64x
    vector-algorithms heapsort:    OK (0.20s)
      1.41 μs ± 117 ns, 2.16x
    fallback heapsort:             OK (0.62s)
      1.15 μs ±  29 ns, 1.76x
  Sorting 10 random arrays of length 17 with few duplicates
    C++:                           OK (0.21s)
      758  ns ±  73 ns
    Sequential ST Median3:         OK (0.38s)
      701  ns ±  35 ns, 0.92x
    Sequential IO Median3:         OK (0.20s)
      728  ns ±  60 ns, 0.96x
    ParStrategies ST Median3:      OK (0.22s)
      773  ns ±  49 ns, 1.02x
    ParStrategies IO Median3:      OK (0.21s)
      766  ns ±  44 ns, 1.01x
    Sequential ST Median3or5:      OK (0.39s)
      727  ns ±  55 ns, 0.96x
    Sequential IO Median3or5:      OK (0.20s)
      724  ns ±  47 ns, 0.95x
    ParStrategies ST Median3or5:   OK (0.22s)
      793  ns ±  68 ns, 1.05x
    ParStrategies IO Median3or5:   OK (0.22s)
      791  ns ±  63 ns, 1.04x
    Sequential ST AveragingMedian: OK (0.30s)
      2.18 μs ±  92 ns, 2.88x
    Threads IO Median3:            OK (0.14s)
      1.89 μs ± 182 ns, 2.49x
    Threads IO Median3or5:         OK (0.14s)
      1.99 μs ± 176 ns, 2.63x
    vector-algorithms heapsort:    OK (0.21s)
      1.52 μs ± 114 ns, 2.01x
    fallback heapsort:             OK (0.18s)
      1.23 μs ± 116 ns, 1.62x
  Sorting 10 random arrays of length 100 with few duplicates
    C++:                           OK (0.16s)
      4.33 μs ± 413 ns
    Sequential ST Median3:         OK (0.38s)
      5.56 μs ± 371 ns, 1.29x
    Sequential IO Median3:         OK (0.20s)
      5.55 μs ± 333 ns, 1.28x
    ParStrategies ST Median3:      OK (0.21s)
      6.03 μs ± 345 ns, 1.39x
    ParStrategies IO Median3:      OK (0.50s)
      7.24 μs ± 287 ns, 1.67x
    Sequential ST Median3or5:      OK (0.20s)
      5.57 μs ± 337 ns, 1.29x
    Sequential IO Median3or5:      OK (0.20s)
      5.59 μs ± 329 ns, 1.29x
    ParStrategies ST Median3or5:   OK (0.22s)
      6.28 μs ± 371 ns, 1.45x
    ParStrategies IO Median3or5:   OK (0.22s)
      6.52 μs ± 621 ns, 1.51x
    Sequential ST AveragingMedian: OK (0.26s)
      30.5 μs ± 2.8 μs, 7.05x
    Threads IO Median3:            OK (0.13s)
      7.28 μs ± 686 ns, 1.68x
    Threads IO Median3or5:         OK (0.49s)
      7.22 μs ± 296 ns, 1.67x
    vector-algorithms heapsort:    OK (0.32s)
      18.8 μs ± 1.4 μs, 4.35x
    fallback heapsort:             OK (0.20s)
      22.9 μs ± 1.5 μs, 5.30x
  Sorting 10 random arrays of length 256 with few duplicates
    C++:                           OK (0.19s)
      44.7 μs ± 2.9 μs
    Sequential ST Median3:         OK (0.45s)
      54.6 μs ± 2.8 μs, 1.22x
    Sequential IO Median3:         OK (0.20s)
      46.9 μs ± 3.5 μs, 1.05x
    ParStrategies ST Median3:      OK (0.22s)
      51.3 μs ± 3.9 μs, 1.15x
    ParStrategies IO Median3:      OK (0.23s)
      53.7 μs ± 4.1 μs, 1.20x
    Sequential ST Median3or5:      OK (0.21s)
      48.1 μs ± 3.1 μs, 1.07x
    Sequential IO Median3or5:      OK (0.21s)
      47.8 μs ± 2.9 μs, 1.07x
    ParStrategies ST Median3or5:   OK (0.23s)
      54.0 μs ± 4.0 μs, 1.21x
    ParStrategies IO Median3or5:   OK (0.24s)
      54.3 μs ± 5.0 μs, 1.21x
    Sequential ST AveragingMedian: OK (0.13s)
      119  μs ±  11 μs, 2.67x
    Threads IO Median3:            OK (0.24s)
      56.3 μs ± 5.6 μs, 1.26x
    Threads IO Median3or5:         OK (0.24s)
      56.1 μs ± 4.6 μs, 1.25x
    vector-algorithms heapsort:    OK (0.23s)
      110  μs ± 6.6 μs, 2.47x
    fallback heapsort:             OK (0.21s)
      95.2 μs ± 6.5 μs, 2.13x
  Sorting 10 random arrays of length 1,000 with few duplicates
    C++:                           OK (0.15s)
      274  μs ±  24 μs
    Sequential ST Median3:         OK (0.13s)
      471  μs ±  46 μs, 1.72x
    Sequential IO Median3:         OK (0.23s)
      428  μs ±  28 μs, 1.56x
    ParStrategies ST Median3:      OK (0.19s)
      350  μs ±  27 μs, 1.27x
    ParStrategies IO Median3:      OK (0.24s)
      451  μs ±  42 μs, 1.65x
    Sequential ST Median3or5:      OK (0.18s)
      333  μs ±  31 μs, 1.22x
    Sequential IO Median3or5:      OK (0.12s)
      428  μs ±  42 μs, 1.56x
    ParStrategies ST Median3or5:   OK (0.25s)
      460  μs ±  42 μs, 1.68x
    ParStrategies IO Median3or5:   OK (0.19s)
      363  μs ±  27 μs, 1.32x
    Sequential ST AveragingMedian: OK (0.20s)
      742  μs ±  64 μs, 2.71x
    Threads IO Median3:            OK (0.25s)
      469  μs ±  25 μs, 1.71x
    Threads IO Median3or5:         OK (0.25s)
      457  μs ±  37 μs, 1.67x
    vector-algorithms heapsort:    OK (0.18s)
      667  μs ±  65 μs, 2.43x
    fallback heapsort:             OK (0.17s)
      618  μs ±  46 μs, 2.25x
  Sorting 10 random arrays of length 10,000 with few duplicates
    C++:                           OK (0.29s)
      4.50 ms ± 269 μs
    Sequential ST Median3:         OK (0.19s)
      6.00 ms ± 586 μs, 1.33x
    Sequential IO Median3:         OK (0.28s)
      4.31 ms ± 260 μs, 0.96x
    ParStrategies ST Median3:      OK (0.18s)
      5.73 ms ± 514 μs, 1.27x
    ParStrategies IO Median3:      OK (0.20s)
      6.06 ms ± 375 μs, 1.35x
    Sequential ST Median3or5:      OK (0.14s)
      4.35 ms ± 353 μs, 0.97x
    Sequential IO Median3or5:      OK (0.18s)
      5.57 ms ± 524 μs, 1.24x
    ParStrategies ST Median3or5:   OK (0.15s)
      4.62 ms ± 340 μs, 1.03x
    ParStrategies IO Median3or5:   OK (0.38s)
      5.89 ms ± 520 μs, 1.31x
    Sequential ST AveragingMedian: OK (0.31s)
      9.89 ms ± 437 μs, 2.20x
    Threads IO Median3:            OK (0.60s)
      4.65 ms ± 291 μs, 1.03x
    Threads IO Median3or5:         OK (0.37s)
      5.77 ms ± 183 μs, 1.28x
    vector-algorithms heapsort:    OK (0.14s)
      8.79 ms ± 718 μs, 1.95x
    fallback heapsort:             OK (0.26s)
      8.34 ms ± 568 μs, 1.85x
  Sorting 10 random arrays of length 100,000 with few duplicates
    C++:                           OK (0.17s)
      55.6 ms ± 3.2 ms
    Sequential ST Median3:         OK (0.24s)
      78.1 ms ± 3.7 ms, 1.41x
    Sequential IO Median3:         OK (0.21s)
      69.3 ms ± 3.7 ms, 1.25x
    ParStrategies ST Median3:      OK (0.18s)
      59.5 ms ± 4.5 ms, 1.07x
    ParStrategies IO Median3:      OK (3.66s)
      56.6 ms ± 5.3 ms, 1.02x
    Sequential ST Median3or5:      OK (0.16s)
      54.0 ms ± 3.5 ms, 0.97x
    Sequential IO Median3or5:      OK (0.21s)
      67.3 ms ± 5.7 ms, 1.21x
    ParStrategies ST Median3or5:   OK (0.19s)
      62.3 ms ± 5.6 ms, 1.12x
    ParStrategies IO Median3or5:   OK (0.45s)
      65.3 ms ± 4.7 ms, 1.17x
    Sequential ST AveragingMedian: OK (0.38s)
      127  ms ± 4.5 ms, 2.28x
    Threads IO Median3:            OK (0.34s)
      48.3 ms ± 2.7 ms, 0.87x
    Threads IO Median3or5:         OK (0.34s)
      47.7 ms ± 2.4 ms, 0.86x
    vector-algorithms heapsort:    OK (0.37s)
      124  ms ± 8.9 ms, 2.23x
    fallback heapsort:             OK (0.81s)
      113  ms ± 5.4 ms, 2.04x
  Sorting 10 random arrays of length 1,000,000 with few duplicates
    C++:                           OK (2.05s)
      682  ms ±  15 ms
    Sequential ST Median3:         OK (2.18s)
      727  ms ± 7.5 ms, 1.06x
    Sequential IO Median3:         OK (2.44s)
      812  ms ± 9.6 ms, 1.19x
    ParStrategies ST Median3:      OK (1.44s)
      204  ms ±  13 ms, 0.30x
    ParStrategies IO Median3:      OK (0.81s)
      268  ms ±  20 ms, 0.39x
    Sequential ST Median3or5:      OK (2.00s)
      664  ms ±  45 ms, 0.97x
    Sequential IO Median3or5:      OK (1.95s)
      645  ms ±  43 ms, 0.95x
    ParStrategies ST Median3or5:   OK (1.49s)
      209  ms ± 4.9 ms, 0.31x
    ParStrategies IO Median3or5:   OK (1.46s)
      208  ms ±  21 ms, 0.30x
    Sequential ST AveragingMedian: OK (4.05s)
      1.348 s ± 8.5 ms, 1.98x
    Threads IO Median3:            OK (1.53s)
      508  ms ± 7.1 ms, 0.74x
    Threads IO Median3or5:         OK (1.36s)
      452  ms ± 6.2 ms, 0.66x
    vector-algorithms heapsort:    OK (3.94s)
      1.309 s ±  53 ms, 1.92x
    fallback heapsort:             OK (3.67s)
      1.220 s ±  27 ms, 1.79x
  Sorting 10 random arrays of length 10,000,000 with few duplicates
    C++:                           OK (18.62s)
      6.188 s ± 240 ms
    Sequential ST Median3:         OK (25.26s)
      8.422 s ±  49 ms, 1.36x
    Sequential IO Median3:         OK (21.91s)
      7.295 s ± 118 ms, 1.18x
    ParStrategies ST Median3:      OK (6.01s)
      2.010 s ± 110 ms, 0.32x
    ParStrategies IO Median3:      OK (6.16s)
      2.044 s ± 100 ms, 0.33x
    Sequential ST Median3or5:      OK (21.85s)
      7.284 s ± 4.9 ms, 1.18x
    Sequential IO Median3or5:      OK (21.94s)
      7.311 s ±  38 ms, 1.18x
    ParStrategies ST Median3or5:   OK (6.05s)
      2.010 s ±  72 ms, 0.32x
    ParStrategies IO Median3or5:   OK (6.27s)
      2.090 s ± 2.8 ms, 0.34x
    Sequential ST AveragingMedian: OK (81.58s)
      27.074 s ± 1.60 s, 4.38x
    Threads IO Median3:            OK (15.86s)
      5.283 s ±  29 ms, 0.85x
    Threads IO Median3or5:         OK (12.85s)
      4.283 s ± 7.3 ms, 0.69x
    vector-algorithms heapsort:    OK (71.14s)
      23.711 s ±  33 ms, 3.83x
    fallback heapsort:             OK (96.39s)
      32.157 s ± 355 ms, 5.20x
  Sorting 10 random arrays of length 16 with many duplicates
    C++:                           OK (0.37s)
      673  ns ±  36 ns
    Sequential ST Median3:         OK (0.17s)
      576  ns ±  53 ns, 0.86x
    Sequential IO Median3:         OK (0.16s)
      560  ns ±  49 ns, 0.83x
    ParStrategies ST Median3:      OK (0.17s)
      576  ns ±  49 ns, 0.86x
    ParStrategies IO Median3:      OK (0.17s)
      575  ns ±  42 ns, 0.85x
    Sequential ST Median3or5:      OK (0.17s)
      575  ns ±  57 ns, 0.85x
    Sequential IO Median3or5:      OK (0.17s)
      568  ns ±  47 ns, 0.84x
    ParStrategies ST Median3or5:   OK (0.33s)
      596  ns ±  50 ns, 0.89x
    ParStrategies IO Median3or5:   OK (0.33s)
      593  ns ±  30 ns, 0.88x
    Sequential ST AveragingMedian: OK (0.32s)
      571  ns ±  26 ns, 0.85x
    Threads IO Median3:            OK (0.24s)
      1.71 μs ±  85 ns, 2.54x
    Threads IO Median3or5:         OK (0.24s)
      1.71 μs ± 118 ns, 2.53x
    vector-algorithms heapsort:    OK (0.20s)
      1.39 μs ±  90 ns, 2.06x
    fallback heapsort:             OK (0.17s)
      1.17 μs ±  83 ns, 1.73x
  Sorting 10 random arrays of length 17 with many duplicates
    C++:                           OK (0.22s)
      763  ns ±  56 ns
    Sequential ST Median3:         OK (0.41s)
      752  ns ±  22 ns, 0.99x
    Sequential IO Median3:         OK (0.21s)
      744  ns ±  64 ns, 0.98x
    ParStrategies ST Median3:      OK (0.23s)
      798  ns ±  44 ns, 1.05x
    ParStrategies IO Median3:      OK (0.22s)
      782  ns ±  46 ns, 1.03x
    Sequential ST Median3or5:      OK (0.21s)
      738  ns ±  44 ns, 0.97x
    Sequential IO Median3or5:      OK (0.21s)
      749  ns ±  52 ns, 0.98x
    ParStrategies ST Median3or5:   OK (0.22s)
      786  ns ±  42 ns, 1.03x
    ParStrategies IO Median3or5:   OK (0.44s)
      796  ns ±  29 ns, 1.04x
    Sequential ST AveragingMedian: OK (0.30s)
      2.16 μs ±  88 ns, 2.83x
    Threads IO Median3:            OK (0.14s)
      1.92 μs ± 173 ns, 2.51x
    Threads IO Median3or5:         OK (0.27s)
      1.98 μs ± 116 ns, 2.59x
    vector-algorithms heapsort:    OK (0.22s)
      1.52 μs ±  97 ns, 1.99x
    fallback heapsort:             OK (0.18s)
      1.26 μs ±  85 ns, 1.65x
  Sorting 10 random arrays of length 100 with many duplicates
    C++:                           OK (0.19s)
      5.12 μs ± 401 ns
    Sequential ST Median3:         OK (0.21s)
      5.85 μs ± 409 ns, 1.14x
    Sequential IO Median3:         OK (0.21s)
      5.85 μs ± 527 ns, 1.14x
    ParStrategies ST Median3:      OK (0.22s)
      6.26 μs ± 395 ns, 1.22x
    ParStrategies IO Median3:      OK (0.28s)
      7.89 μs ± 461 ns, 1.54x
    Sequential ST Median3or5:      OK (0.21s)
      5.71 μs ± 458 ns, 1.11x
    Sequential IO Median3or5:      OK (0.20s)
      5.70 μs ± 404 ns, 1.11x
    ParStrategies ST Median3or5:   OK (0.23s)
      6.45 μs ± 381 ns, 1.26x
    ParStrategies IO Median3or5:   OK (0.23s)
      6.47 μs ± 499 ns, 1.26x
    Sequential ST AveragingMedian: OK (0.13s)
      29.0 μs ± 2.6 μs, 5.65x
    Threads IO Median3:            OK (0.14s)
      7.34 μs ± 671 ns, 1.43x
    Threads IO Median3or5:         OK (0.26s)
      7.48 μs ± 455 ns, 1.46x
    vector-algorithms heapsort:    OK (0.17s)
      18.6 μs ± 1.3 μs, 3.63x
    fallback heapsort:             OK (0.20s)
      23.2 μs ± 2.2 μs, 4.52x
  Sorting 10 random arrays of length 256 with many duplicates
    C++:                           OK (0.20s)
      46.3 μs ± 3.2 μs
    Sequential ST Median3:         OK (0.25s)
      55.6 μs ± 5.0 μs, 1.20x
    Sequential IO Median3:         OK (0.42s)
      49.5 μs ± 3.0 μs, 1.07x
    ParStrategies ST Median3:      OK (0.23s)
      53.0 μs ± 2.7 μs, 1.14x
    ParStrategies IO Median3:      OK (0.13s)
      55.7 μs ± 5.3 μs, 1.20x
    Sequential ST Median3or5:      OK (0.22s)
      49.2 μs ± 2.9 μs, 1.06x
    Sequential IO Median3or5:      OK (0.42s)
      49.3 μs ± 2.0 μs, 1.06x
    ParStrategies ST Median3or5:   OK (0.24s)
      55.2 μs ± 4.2 μs, 1.19x
    ParStrategies IO Median3or5:   OK (0.47s)
      55.0 μs ± 1.5 μs, 1.19x
    Sequential ST AveragingMedian: OK (0.13s)
      121  μs ±  11 μs, 2.60x
    Threads IO Median3:            OK (0.13s)
      56.1 μs ± 5.4 μs, 1.21x
    Threads IO Median3or5:         OK (0.25s)
      56.6 μs ± 3.4 μs, 1.22x
    vector-algorithms heapsort:    OK (0.22s)
      102  μs ± 5.2 μs, 2.21x
    fallback heapsort:             OK (0.21s)
      94.9 μs ± 5.4 μs, 2.05x
  Sorting 10 random arrays of length 1,000 with many duplicates
    C++:                           OK (0.30s)
      279  μs ±  14 μs
    Sequential ST Median3:         OK (0.20s)
      367  μs ±  22 μs, 1.31x
    Sequential IO Median3:         OK (0.18s)
      335  μs ±  33 μs, 1.20x
    ParStrategies ST Median3:      OK (0.19s)
      344  μs ±  22 μs, 1.23x
    ParStrategies IO Median3:      OK (0.19s)
      359  μs ±  24 μs, 1.28x
    Sequential ST Median3or5:      OK (0.18s)
      336  μs ±  25 μs, 1.20x
    Sequential IO Median3or5:      OK (0.18s)
      336  μs ±  21 μs, 1.20x
    ParStrategies ST Median3or5:   OK (0.38s)
      358  μs ±  14 μs, 1.28x
    ParStrategies IO Median3or5:   OK (0.19s)
      363  μs ±  26 μs, 1.30x
    Sequential ST AveragingMedian: OK (0.16s)
      583  μs ±  43 μs, 2.09x
    Threads IO Median3:            OK (0.19s)
      357  μs ±  23 μs, 1.28x
    Threads IO Median3or5:         OK (0.19s)
      358  μs ±  28 μs, 1.28x
    vector-algorithms heapsort:    OK (0.15s)
      536  μs ±  42 μs, 1.92x
    fallback heapsort:             OK (0.13s)
      494  μs ±  46 μs, 1.77x
  Sorting 10 random arrays of length 10,000 with many duplicates
    C++:                           OK (0.21s)
      3.25 ms ± 184 μs
    Sequential ST Median3:         OK (0.14s)
      4.23 ms ± 347 μs, 1.30x
    Sequential IO Median3:         OK (0.24s)
      3.65 ms ± 329 μs, 1.12x
    ParStrategies ST Median3:      OK (0.12s)
      3.76 ms ± 362 μs, 1.15x
    ParStrategies IO Median3:      OK (0.13s)
      3.98 ms ± 360 μs, 1.22x
    Sequential ST Median3or5:      OK (0.24s)
      3.72 ms ± 254 μs, 1.14x
    Sequential IO Median3or5:      OK (0.12s)
      3.71 ms ± 344 μs, 1.14x
    ParStrategies ST Median3or5:   OK (0.13s)
      3.96 ms ± 344 μs, 1.22x
    ParStrategies IO Median3or5:   OK (0.27s)
      4.10 ms ± 315 μs, 1.26x
    Sequential ST AveragingMedian: OK (0.12s)
      7.81 ms ± 724 μs, 2.40x
    Threads IO Median3:            OK (0.13s)
      3.92 ms ± 338 μs, 1.20x
    Threads IO Median3or5:         OK (0.13s)
      4.05 ms ± 347 μs, 1.24x
    vector-algorithms heapsort:    OK (0.12s)
      7.85 ms ± 674 μs, 2.41x
    fallback heapsort:             OK (0.21s)
      6.58 ms ± 434 μs, 2.02x
  Sorting 10 random arrays of length 100,000 with many duplicates
    C++:                           OK (0.24s)
      33.0 ms ± 1.5 ms
    Sequential ST Median3:         OK (0.12s)
      38.8 ms ± 2.7 ms, 1.18x
    Sequential IO Median3:         OK (0.10s)
      33.1 ms ± 2.9 ms, 1.00x
    ParStrategies ST Median3:      OK (0.23s)
      32.9 ms ± 1.9 ms, 1.00x
    ParStrategies IO Median3:      OK (0.13s)
      41.5 ms ± 3.3 ms, 1.26x
    Sequential ST Median3or5:      OK (0.23s)
      32.8 ms ± 2.3 ms, 1.00x
    Sequential IO Median3or5:      OK (0.10s)
      33.4 ms ± 3.1 ms, 1.01x
    ParStrategies ST Median3or5:   OK (0.23s)
      32.9 ms ± 1.5 ms, 1.00x
    ParStrategies IO Median3or5:   OK (0.23s)
      33.0 ms ± 2.0 ms, 1.00x
    Sequential ST AveragingMedian: OK (0.31s)
      102  ms ± 7.6 ms, 3.08x
    Threads IO Median3:            OK (0.48s)
      32.2 ms ± 982 μs, 0.98x
    Threads IO Median3or5:         OK (0.48s)
      31.3 ms ± 1.6 ms, 0.95x
    vector-algorithms heapsort:    OK (0.31s)
      103  ms ± 5.2 ms, 3.12x
    fallback heapsort:             OK (0.27s)
      89.0 ms ± 4.7 ms, 2.70x
  Sorting 10 random arrays of length 1,000,000 with many duplicates
    C++:                           OK (1.03s)
      339  ms ±  32 ms
    Sequential ST Median3:         OK (1.16s)
      387  ms ± 5.5 ms, 1.14x
    Sequential IO Median3:         OK (0.99s)
      329  ms ± 5.0 ms, 0.97x
    ParStrategies ST Median3:      OK (0.59s)
      198  ms ±  15 ms, 0.58x
    ParStrategies IO Median3:      OK (0.60s)
      198  ms ± 5.8 ms, 0.58x
    Sequential ST Median3or5:      OK (0.99s)
      329  ms ± 8.3 ms, 0.97x
    Sequential IO Median3or5:      OK (1.00s)
      331  ms ± 3.6 ms, 0.98x
    ParStrategies ST Median3or5:   OK (1.39s)
      199  ms ± 5.0 ms, 0.59x
    ParStrategies IO Median3or5:   OK (0.60s)
      199  ms ± 3.0 ms, 0.59x
    Sequential ST AveragingMedian: OK (3.82s)
      1.273 s ±  25 ms, 3.76x
    Threads IO Median3:            OK (0.88s)
      294  ms ±  27 ms, 0.87x
    Threads IO Median3or5:         OK (1.92s)
      274  ms ±  15 ms, 0.81x
    vector-algorithms heapsort:    OK (11.77s)
      1.664 s ± 6.3 ms, 4.91x
    fallback heapsort:             OK (4.08s)
      1.357 s ±  23 ms, 4.00x
  Sorting 10 random arrays of length 10,000,000 with many duplicates
    C++:                           OK (16.77s)
      5.595 s ±  88 ms
    Sequential ST Median3:         OK (15.18s)
      5.059 s ± 9.0 ms, 0.90x
    Sequential IO Median3:         OK (16.64s)
      5.541 s ±  75 ms, 0.99x
    ParStrategies ST Median3:      OK (4.50s)
      1.499 s ± 4.0 ms, 0.27x
    ParStrategies IO Median3:      OK (4.60s)
      1.534 s ±  14 ms, 0.27x
    Sequential ST Median3or5:      OK (12.89s)
      4.287 s ± 114 ms, 0.77x
    Sequential IO Median3or5:      OK (12.81s)
      4.267 s ±  46 ms, 0.76x
    ParStrategies ST Median3or5:   OK (4.19s)
      1.399 s ±  70 ms, 0.25x
    ParStrategies IO Median3or5:   OK (4.28s)
      1.430 s ±  43 ms, 0.26x
    Sequential ST AveragingMedian: OK (72.72s)
      24.214 s ± 367 ms, 4.33x
    Threads IO Median3:            OK (10.38s)
      3.450 s ± 125 ms, 0.62x
    Threads IO Median3or5:         OK (8.70s)
      2.886 s ± 191 ms, 0.52x
    vector-algorithms heapsort:    OK (68.36s)
      22.767 s ± 265 ms, 4.07x
    fallback heapsort:             OK (65.38s)
      21.792 s ± 5.9 ms, 3.90x
```


# Symbolic Gossip
_Symbolic Model Checking of the Gossip Problem_



See [report.pdf](report.pdf) for documentation.

## Benchmarks
The following benchmarks are most recently produced.
To run the benchmarks, run `stack ghci bench/TrfBenchmarks.lhs` from the root folder and run `main --time-limit=60` in the GHCi.
See [bench/TrfBenchmarks.lhs](bench/TrfBenchmarks.lhs) for further documentation.
```
benchmarking SmpTrf - 3 agents/1 call
time                 180.8 μs   (170.0 μs .. 196.8 μs)
                     0.962 R²   (0.948 R² .. 0.977 R²)
mean                 186.6 μs   (178.7 μs .. 197.6 μs)
std dev              30.70 μs   (25.61 μs .. 43.76 μs)
variance introduced by outliers: 92% (severely inflated)

benchmarking SmpTrf - 3 agents/3 calls
time                 479.4 μs   (458.9 μs .. 502.7 μs)
                     0.976 R²   (0.953 R² .. 0.993 R²)
mean                 455.4 μs   (439.6 μs .. 484.0 μs)
std dev              67.31 μs   (45.51 μs .. 104.5 μs)
variance introduced by outliers: 88% (severely inflated)

benchmarking SmpTrf - 3 agents/5 calls
time                 429.0 μs   (424.2 μs .. 434.4 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 422.4 μs   (418.3 μs .. 427.8 μs)
std dev              15.56 μs   (11.23 μs .. 23.58 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking SmpTrf - 5 agents/1 call
time                 1.155 ms   (1.137 ms .. 1.183 ms)
                     0.993 R²   (0.981 R² .. 0.999 R²)
mean                 1.134 ms   (1.119 ms .. 1.170 ms)
std dev              74.75 μs   (37.82 μs .. 139.2 μs)
variance introduced by outliers: 53% (severely inflated)

benchmarking SmpTrf - 5 agents/3 calls
time                 1.306 ms   (1.285 ms .. 1.331 ms)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 1.190 ms   (1.163 ms .. 1.218 ms)
std dev              90.42 μs   (80.30 μs .. 105.1 μs)
variance introduced by outliers: 59% (severely inflated)

benchmarking SmpTrf - 5 agents/5 calls
time                 3.451 ms   (3.191 ms .. 3.746 ms)
                     0.876 R²   (0.762 R² .. 0.957 R²)
mean                 3.973 ms   (3.694 ms .. 4.418 ms)
std dev              1.059 ms   (762.2 μs .. 1.645 ms)
variance introduced by outliers: 93% (severely inflated)

benchmarking SmpTrf - 7 agents/1 call
time                 4.395 ms   (4.240 ms .. 4.557 ms)
                     0.993 R²   (0.989 R² .. 0.997 R²)
mean                 4.288 ms   (4.221 ms .. 4.370 ms)
std dev              233.0 μs   (183.6 μs .. 327.8 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking SmpTrf - 7 agents/3 calls
time                 4.212 ms   (4.147 ms .. 4.278 ms)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 4.316 ms   (4.263 ms .. 4.384 ms)
std dev              195.9 μs   (151.2 μs .. 261.8 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking SmpTrf - 7 agents/5 calls
time                 14.76 ms   (12.74 ms .. 17.95 ms)
                     0.854 R²   (0.760 R² .. 0.960 R²)
mean                 14.74 ms   (13.75 ms .. 16.04 ms)
std dev              2.914 ms   (2.147 ms .. 4.028 ms)
variance introduced by outliers: 79% (severely inflated)

benchmarking ClsTrf - 3 agents/1 call
time                 637.4 μs   (622.8 μs .. 667.5 μs)
                     0.966 R²   (0.937 R² .. 0.989 R²)
mean                 660.8 μs   (632.3 μs .. 707.5 μs)
std dev              122.3 μs   (81.01 μs .. 169.0 μs)
variance introduced by outliers: 92% (severely inflated)

benchmarking ClsTrf - 3 agents/3 calls
time                 1.819 ms   (1.534 ms .. 2.019 ms)
                     0.899 R²   (0.858 R² .. 0.936 R²)
mean                 1.568 ms   (1.437 ms .. 1.706 ms)
std dev              425.0 μs   (382.1 μs .. 454.8 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking ClsTrf - 3 agents/5 calls
time                 1.912 ms   (1.858 ms .. 1.959 ms)
                     0.991 R²   (0.985 R² .. 0.995 R²)
mean                 1.975 ms   (1.903 ms .. 2.120 ms)
std dev              351.6 μs   (226.9 μs .. 495.5 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking ClsTrf - 5 agents/1 call
time                 22.22 ms   (21.92 ms .. 22.58 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 22.38 ms   (22.16 ms .. 22.72 ms)
std dev              659.3 μs   (404.3 μs .. 1.058 ms)

benchmarking ClsTrf - 5 agents/3 calls
time                 48.44 ms   (44.71 ms .. 53.16 ms)
                     0.979 R²   (0.952 R² .. 0.996 R²)
mean                 45.65 ms   (44.32 ms .. 48.27 ms)
std dev              3.420 ms   (2.161 ms .. 4.928 ms)
variance introduced by outliers: 27% (moderately inflated)

benchmarking ClsTrf - 5 agents/5 calls
time                 68.75 ms   (64.96 ms .. 72.79 ms)
                     0.995 R²   (0.992 R² .. 0.999 R²)
mean                 70.74 ms   (68.84 ms .. 72.11 ms)
std dev              2.849 ms   (1.822 ms .. 4.162 ms)

benchmarking ClsTrf - 7 agents/1 call
time                 557.9 ms   (554.3 ms .. 563.5 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 561.3 ms   (559.6 ms .. 562.8 ms)
std dev              1.787 ms   (1.373 ms .. 2.102 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking ClsTrf - 7 agents/3 calls
time                 1.191 s    (1.128 s .. 1.221 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.280 s    (1.236 s .. 1.326 s)
std dev              50.40 ms   (16.73 ms .. 68.97 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking ClsTrf - 7 agents/5 calls
time                 11.02 s    (10.08 s .. 11.50 s)
                     0.999 R²   (NaN R² .. 1.000 R²)
mean                 11.02 s    (10.86 s .. 11.15 s)
std dev              167.1 ms   (80.43 ms .. 220.0 ms)
variance introduced by outliers: 19% (moderately inflated)

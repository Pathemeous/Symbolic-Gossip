# Symbolic Gossip
_Symbolic Model Checking of the Gossip Problem_

## Running the code
To run the code and see how specific transformers withspecific call sequences result in different knowledge 
scenes you can run the following: 

```
stack build 
stack ghci exec/Main.hs
```

from the main directory. Then in ghci: 

```
ghci> main
```

Then you can enter the number of agents in the gossip scenario you want to model, after which you can choose which 
knowledge transformer to use. 

After this you will see a description of the initial gossip scene. After which you can input calls between agents
(agents ranging from 0 up to the amount given as previous input).  


See [report.pdf](report.pdf) for documentation.

## Benchmarks
The following benchmarks are most recently produced.
To run the benchmarks, run `stack bench` from the root folder
See [bench/trfBenchmarks.hs](bench/trfBenchmarks.hs) for further documentation.
```
benchmarking SmpTrf - 3 agents/1 call
time                 91.61 μs   (84.94 μs .. 97.30 μs)
                     0.975 R²   (0.960 R² .. 0.988 R²)
mean                 88.29 μs   (85.05 μs .. 93.54 μs)
std dev              14.18 μs   (10.70 μs .. 20.56 μs)
variance introduced by outliers: 92% (severely inflated)

benchmarking SmpTrf - 3 agents/3 calls
time                 100.8 μs   (96.63 μs .. 104.8 μs)
                     0.982 R²   (0.973 R² .. 0.990 R²)
mean                 99.34 μs   (95.63 μs .. 103.9 μs)
std dev              13.86 μs   (11.42 μs .. 17.02 μs)
variance introduced by outliers: 90% (severely inflated)

benchmarking SmpTrf - 3 agents/5 calls
time                 475.9 μs   (439.2 μs .. 513.0 μs)
                     0.959 R²   (0.935 R² .. 0.978 R²)
mean                 486.2 μs   (458.0 μs .. 514.8 μs)
std dev              91.05 μs   (76.93 μs .. 111.1 μs)
variance introduced by outliers: 93% (severely inflated)

benchmarking SmpTrf - 4 agents/1 call
time                 331.5 μs   (307.4 μs .. 355.1 μs)
                     0.947 R²   (0.900 R² .. 0.979 R²)
mean                 289.8 μs   (273.9 μs .. 317.2 μs)
std dev              66.15 μs   (39.68 μs .. 111.4 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking SmpTrf - 4 agents/3 calls
time                 1.136 ms   (1.057 ms .. 1.213 ms)
                     0.960 R²   (0.938 R² .. 0.977 R²)
mean                 995.6 μs   (942.0 μs .. 1.052 ms)
std dev              174.4 μs   (148.5 μs .. 210.5 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking SmpTrf - 4 agents/5 calls
time                 784.1 μs   (749.8 μs .. 847.3 μs)
                     0.919 R²   (0.875 R² .. 0.959 R²)
mean                 976.6 μs   (913.5 μs .. 1.056 ms)
std dev              234.2 μs   (203.3 μs .. 267.9 μs)
variance introduced by outliers: 94% (severely inflated)

benchmarking SmpTrf - 5 agents/1 call
time                 646.4 μs   (633.2 μs .. 663.3 μs)
                     0.996 R²   (0.994 R² .. 0.999 R²)
mean                 638.0 μs   (630.9 μs .. 649.1 μs)
std dev              29.30 μs   (22.60 μs .. 37.87 μs)
variance introduced by outliers: 38% (moderately inflated)

benchmarking SmpTrf - 5 agents/3 calls
time                 2.447 ms   (2.187 ms .. 2.754 ms)
                     0.923 R²   (0.887 R² .. 0.962 R²)
mean                 2.153 ms   (2.035 ms .. 2.316 ms)
std dev              427.8 μs   (323.7 μs .. 546.3 μs)
variance introduced by outliers: 90% (severely inflated)

benchmarking SmpTrf - 5 agents/5 calls
time                 2.167 ms   (2.073 ms .. 2.265 ms)
                     0.982 R²   (0.968 R² .. 0.993 R²)
mean                 2.096 ms   (2.037 ms .. 2.187 ms)
std dev              234.3 μs   (174.3 μs .. 380.6 μs)
variance introduced by outliers: 74% (severely inflated)

benchmarking ClsTrf - 3 agents/1 call
time                 437.5 μs   (404.4 μs .. 456.3 μs)
                     0.977 R²   (0.971 R² .. 0.985 R²)
mean                 388.4 μs   (376.8 μs .. 402.8 μs)
std dev              43.75 μs   (36.27 μs .. 51.77 μs)
variance introduced by outliers: 81% (severely inflated)

benchmarking ClsTrf - 3 agents/3 calls
time                 985.4 μs   (957.6 μs .. 1.008 ms)
                     0.955 R²   (0.895 R² .. 0.986 R²)
mean                 1.308 ms   (1.227 ms .. 1.405 ms)
std dev              323.5 μs   (260.1 μs .. 469.4 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking ClsTrf - 3 agents/5 calls
time                 2.033 ms   (1.924 ms .. 2.171 ms)
                     0.976 R²   (0.956 R² .. 0.992 R²)
mean                 1.876 ms   (1.834 ms .. 1.961 ms)
std dev              191.9 μs   (139.1 μs .. 314.8 μs)
variance introduced by outliers: 69% (severely inflated)

benchmarking ClsTrf - 4 agents/1 call
time                 2.847 ms   (2.591 ms .. 3.161 ms)
                     0.946 R²   (0.910 R² .. 0.980 R²)
mean                 2.736 ms   (2.617 ms .. 2.876 ms)
std dev              425.0 μs   (357.2 μs .. 627.7 μs)
variance introduced by outliers: 83% (severely inflated)

benchmarking ClsTrf - 4 agents/3 calls
time                 9.385 ms   (8.941 ms .. 9.773 ms)
                     0.968 R²   (0.930 R² .. 0.988 R²)
mean                 8.423 ms   (8.039 ms .. 8.900 ms)
std dev              1.189 ms   (876.5 μs .. 1.739 ms)
variance introduced by outliers: 71% (severely inflated)

benchmarking ClsTrf - 4 agents/5 calls
time                 12.53 ms   (9.774 ms .. 15.15 ms)
                     0.754 R²   (0.475 R² .. 0.952 R²)
mean                 19.57 ms   (17.57 ms .. 23.15 ms)
std dev              6.303 ms   (3.938 ms .. 8.712 ms)
variance introduced by outliers: 91% (severely inflated)

benchmarking ClsTrf - 5 agents/1 call
time                 17.66 ms   (12.66 ms .. 23.56 ms)
                     0.740 R²   (0.583 R² .. 0.920 R²)
mean                 18.33 ms   (16.58 ms .. 21.01 ms)
std dev              4.837 ms   (3.220 ms .. 7.050 ms)
variance introduced by outliers: 86% (severely inflated)

benchmarking ClsTrf - 5 agents/3 calls
time                 37.18 ms   (24.98 ms .. 43.50 ms)
                     0.750 R²   (0.322 R² .. 0.960 R²)
mean                 63.01 ms   (52.45 ms .. 79.43 ms)
std dev              25.10 ms   (15.74 ms .. 36.58 ms)
variance introduced by outliers: 92% (severely inflated)

benchmarking ClsTrf - 5 agents/5 calls
time                 12.81 s    (12.00 s .. 14.09 s)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 12.27 s    (12.10 s .. 12.55 s)
std dev              276.2 ms   (33.61 ms .. 351.3 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking TnsTrf - 3 agents/1 call
time                 148.0 μs   (146.3 μs .. 149.8 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 146.0 μs   (144.9 μs .. 147.1 μs)
std dev              3.788 μs   (3.073 μs .. 4.679 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking TnsTrf - 3 agents/3 calls
time                 293.6 μs   (289.7 μs .. 298.3 μs)
                     0.991 R²   (0.983 R² .. 0.996 R²)
mean                 331.4 μs   (319.3 μs .. 347.0 μs)
std dev              48.50 μs   (39.34 μs .. 63.12 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking TnsTrf - 3 agents/5 calls
time                 524.9 μs   (506.5 μs .. 542.4 μs)
                     0.986 R²   (0.973 R² .. 0.995 R²)
mean                 491.2 μs   (477.7 μs .. 511.9 μs)
std dev              52.83 μs   (34.29 μs .. 87.06 μs)
variance introduced by outliers: 79% (severely inflated)

benchmarking TnsTrf - 4 agents/1 call
time                 273.1 μs   (255.5 μs .. 292.6 μs)
                     0.977 R²   (0.969 R² .. 0.990 R²)
mean                 265.8 μs   (259.3 μs .. 274.1 μs)
std dev              24.61 μs   (17.88 μs .. 34.30 μs)
variance introduced by outliers: 76% (severely inflated)

benchmarking TnsTrf - 4 agents/3 calls
time                 842.4 μs   (831.4 μs .. 855.0 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 830.4 μs   (823.7 μs .. 841.1 μs)
std dev              26.04 μs   (17.67 μs .. 44.81 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking TnsTrf - 4 agents/5 calls
time                 1.193 ms   (1.112 ms .. 1.324 ms)
                     0.942 R²   (0.907 R² .. 0.976 R²)
mean                 1.433 ms   (1.377 ms .. 1.486 ms)
std dev              194.9 μs   (166.4 μs .. 230.8 μs)
variance introduced by outliers: 82% (severely inflated)

benchmarking TnsTrf - 5 agents/1 call
time                 489.7 μs   (467.9 μs .. 508.6 μs)
                     0.983 R²   (0.971 R² .. 0.992 R²)
mean                 477.0 μs   (462.8 μs .. 494.9 μs)
std dev              54.69 μs   (44.04 μs .. 82.83 μs)
variance introduced by outliers: 81% (severely inflated)

benchmarking TnsTrf - 5 agents/3 calls
time                 1.299 ms   (1.220 ms .. 1.370 ms)
                     0.979 R²   (0.969 R² .. 0.988 R²)
mean                 1.323 ms   (1.274 ms .. 1.389 ms)
std dev              192.9 μs   (149.3 μs .. 254.0 μs)
variance introduced by outliers: 84% (severely inflated)

benchmarking TnsTrf - 5 agents/5 calls
time                 1.845 ms   (1.691 ms .. 2.010 ms)
                     0.966 R²   (0.951 R² .. 0.986 R²)
mean                 1.831 ms   (1.779 ms .. 1.913 ms)
std dev              216.7 μs   (164.0 μs .. 335.5 μs)
variance introduced by outliers: 77% (severely inflated)
```

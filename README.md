## `bytes16`

This library implements encoding and decoding a [`ByteArray`](http://hackage.haskell.org/package/primitive-0.7.0.1/docs/Data-Primitive-ByteArray.html#t:ByteArray) into base-16.

We make use of primitive GHC operations including simd operations that GHC
currently supports with the llvm backend in order to try to extract the highest performance.

### Explanation:

A base-16 encoding takes a stream of bytes which we might think of as base-256
and converts it into an ascii representation of hexadecimal:

```terminal

                    ┌───────┐   ┌───────┐   ┌───────┐
             ··· ───│  129  │───│  188  │───│  15   │───···
                    └───────┘   └───────┘   └───────┘

                              ||         ||
                              || encode  ||
                              ||         ||

        ┌───────┐   ┌───────┐   ┌───────┐   ┌───────┐   ┌───────┐   ┌───────┐
 ··· ───│  '8'  │───│  '1'  │───│  'b'  │───│  'c'  │───│  '0'  │───│  'f'  │───···
        └───────┘   └───────┘   └───────┘   └───────┘   └───────┘   └───────┘
```

Note here that:
```
129 = 8  * 16  + 1
188 = 11 * 16  + 12
15  = 0  * 16  + 15
```

### Benchmarks:
There are currently two other libraries that provide these encodings:
  * [base16-bytestring](https://hackage.haskell.org/package/base16-bytestring)
  * [base16](https://hackage.haskell.org/package/base16)

Here are the initial benchmarks showing this library's performance compared to these two:
```terminal
benchmarking base16 benchmarks:/bytes16-encode
time                 8.164 μs   (8.103 μs .. 8.222 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 8.139 μs   (8.097 μs .. 8.189 μs)
std dev              155.5 ns   (128.2 ns .. 194.9 ns)
variance introduced by outliers: 18% (moderately inflated)

benchmarking base16 benchmarks:/base16-bytestring-encode
time                 38.95 μs   (38.75 μs .. 39.18 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 38.90 μs   (38.71 μs .. 39.16 μs)
std dev              771.0 ns   (542.0 ns .. 1.220 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking base16 benchmarks:/base16-encode
time                 9.186 μs   (9.121 μs .. 9.292 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 9.289 μs   (9.185 μs .. 9.493 μs)
std dev              462.8 ns   (263.1 ns .. 700.3 ns)
variance introduced by outliers: 61% (severely inflated)

benchmarking base16 benchmarks:/bytes16-decode-encode
time                 14.64 μs   (14.56 μs .. 14.75 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 14.58 μs   (14.52 μs .. 14.66 μs)
std dev              235.3 ns   (181.6 ns .. 368.0 ns)
variance introduced by outliers: 13% (moderately inflated)

benchmarking base16 benchmarks:/base16-bytestring-decode-encode
time                 94.45 μs   (93.68 μs .. 95.26 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 95.98 μs   (94.88 μs .. 98.01 μs)
std dev              4.762 μs   (2.825 μs .. 8.233 μs)
variance introduced by outliers: 52% (severely inflated)

benchmarking base16 benchmarks:/base16-decode-encode
time                 21.86 μs   (21.52 μs .. 22.18 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 21.61 μs   (21.46 μs .. 21.77 μs)
std dev              509.8 ns   (395.2 ns .. 761.4 ns)
variance introduced by outliers: 23% (moderately inflated)
```
In particular, encoding and decoding is about 4-6x faster than the base16-bytestring library.

_work in progress_

# APLAcc

*APL to Accelerate compiler*

## Installation

Clone and run the following command in the toplevel directory:
```shell
$ cabal install
```

## Development installation

```shell
$ cabal install --only-dependencies
$ cabal configure
$ cabal build
$ export PATH=$PATH:$PWD/dist/build/aplacc/aplacc
```

## Usage
```
Usage: aplacc [options] <file>

  -v, --verbose          verbose output
  -t, --tail             input file is a TAIL program
  -c, --cuda             use CUDA backend
  --run                  run Accelerate program
```

To compile and run a TAIL program:
```shell
$ aplacc --tail program.tail > program.hs
$ ghc -threaded program.hs
$ ./program
```

To compile and run an APL program:
```shell
$ aplacc program.apl > program.hs
$ ghc -threaded program.hs
$ ./program
```
This requires that the [`aplt`](http://github.com/melsmann/apltail) compiler is available, either on your `PATH` or that the `APLT` environment variable contains the path to the compiler.
You can optionally use the environment variable `APLT_PRELUDE` to specify an APL file containing prelude function definitions.

With the `--run` flag, the Accelerate program is run with the `runghc` program instead of being printed.
```shell
$ aplacc --run program.apl
```
For this to work APLAcc must be installed with cabal or the environment variable `APLACC_HOME` must point to the directory containing `aplacc.cabal`. 

## Testing

The repository contains a number of test programs. The test programs can be run using the `test.sh` script:
```shell
$ ./test.sh tests/working               # Run all programs in the directory
$ ./test.sh tests/working/life.tail -v  # Run specific program verbosely
```

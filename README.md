# qcolor
[![Build Status](https://travis-ci.org/0l-l0/qcolor.svg?branch=master)](https://travis-ci.org/0l-l0/qcolor)
[![codecov](https://codecov.io/gh/0l-l0/qcolor/branch/master/graph/badge.svg)](https://codecov.io/gh/0l-l0/qcolor)

qcolor is a greedy graph quasi-coloring algorithm which can be used to
calculate a lower bound for the _k_ parameter of the feasible _k_-colorings of
a finite simple graph, furthermore, it also gives an upper bound for the
maximum clique of the given graph.

This algorithm is not unique in calculating the two above-mentioned bounds but
it holds two really advantageous property, namely, it runs in polynomial time
and it is very easily parallelizable.

A more detailed description of the algorithm can be found in the original
article of Szabó and Zaválnij [1] and the `doc` folder also contains a
commented example run.

This repository contains an Ada implementation using OpenCL kernels for the
actual work.

## Dependencies
* GNAT compiler + [gprbuild](https://github.com/AdaCore/gprbuild)
* working OpenCL implementation and a compatible hardware
* [OpenCLAda](http://flyx.github.io/OpenCLAda/) (Ada binding for OpenCL)

## Build instructions
> These instructions are only tested on Linux but theoretically they should
> work in any POSIX-compliant shell even on Windows.

First of all you need to specify the project location of the OpenCLAda library.
This can be done by setting the value of `GPR_PROJECT_PATH`:

```sh
export GPR_PROJECT_PATH
```

The default value of this variable is `./ext/OpenCLAda` so the easiest way to
make the compiler recognize the OpenCL binding library is to install it
directly into `./ext/` relative to the project root directory. (Note that you
can add further libraries through this variable by appending more pathnames
with a library separator to it.)

After you are confident the `GPR_PROJECT_PATH` variable is set correctly simply
run:

```sh
./build.sh
```

If everything works well, you will get an executable in the `bin` directory. To
start fooling around you may want to read the help message first.

```sh
./bin/qcolor -h
```

## License
This software is distributed under the terms and conditions of the GNU GPL v3.0. 
Copyright &copy; 2019, 0l-l0

## References
* [1] Szabó, Sándor & Zaválnij, Bogdán. (2018). Decomposing clique search problems
into smaller instances based on node and edge colorings. Discrete Applied
Mathematics. 242. 10.1016/j.dam.2018.01.006.

# Co-Path-Set-Algorithm-Implementation
An implementation to the algorithm described in this paper: https://www.sciencedirect.com/science/article/pii/S0020019022000928
This will, deterministically, solves the Co-path Set problem in O(2^k) time.
For more information, please visit https://sites.google.com/nyu.edu/co-path-set-implementation/

# How to run
The program has 2 usages:

1. ./co-path --random {probability of an edge} {number of vertices} {k}
2. ./co-path {file name} {k}
3. ./co-path --csv {csv file} {k}

In any case, the program will print out the number of edges in the delete set and list them. Also,
it will draw 2 pictures. One is before running the algorithm, the other is after running the algorithm. 
Typically, the image after running the algorithm is placed on top and has "<2>" on the name.


You should provide the file that is formatted like the following if you choose text file input (i.e., option 2):

{number of vertices}

for each edge:

{edge start} {edge end}


Some example input files is provided. Please ensure there are no additional whitespace between lines as demonstrated in the
example. Vertices are identified by a number 1..[number of vertices]. The parser will enforce all integers within this
range to be inserted into the graph, even if there are no edges connected to it.

The parsing capability is rather basic and cannot handle variations of syntax. Please follow the example file's syntax to
prevent parsing errors, which are typically shown as "*** Exception: co-path.hs:12:7-34: Non-exhaustive patterns in [from, to]"


Please visit https://hackage.haskell.org/package/graphite-0.9.6.0/docs/Data-Graph-Read.html for the format of the input
CSV file if you choose to do so.

# Dependencies
"graphite" library and graphviz are required in addition of GHC Standard Library.
In terminal, use "cabal install --lib graphite" to install graphite and "sudo apt install graphviz" to install graphviz


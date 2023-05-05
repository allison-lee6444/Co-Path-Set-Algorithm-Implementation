# Co-Path-Set-Algorithm-Implementation
An implementation to the algorithm described in this paper: https://www.sciencedirect.com/science/article/pii/S0020019022000928
This will, deterministically, solves the Co-path Set problem in O(2^k) time.
For more information, please visit https://sites.google.com/nyu.edu/co-path-set-implementation/

# How to run
The program will first prompt user for a file containing the graph as the input. The user should provide the file
that is formatted like the following:

{number of vertices}
for each edge:
{edge start} {edge end}


An example input file is provided. Please ensure there are no additional whitespace between lines as demonstrated in the
example. Vertices are identified by a number 1..[number of vertices]. The parser will enforce all integers within this
range to be inserted into the graph, even if there are no edges connected to it.

The parsing capability is rather basic and cannot handle variations of syntax. Please follow the example file's syntax to
prevent parsing errors, which are typically shown as "*** Exception: co-path.hs:12:7-34: Non-exhaustive patterns in [from, to]"

# Dependencies
"graphite" library is required in addition of GHC Standard Library.
In terminal, "cabal install --lib graphite" to install graphite

# Acknowledgement:


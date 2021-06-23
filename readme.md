# Precedence Constraints Analyzer. 
Given a TSPLIB-compatible input file (.sop), encoding an instance of Precedence Constrained TSP (same as Sequential Ordering Problem), compute the _width_ of the partial order given by the precedence constraints, as well as its _transitive reduction_ and _density_.

I wrote it to estimate if it was feasible to solve specific instances of TSPLIB, and used it in (Salii, 2017)[https://www.researchgate.net/publication/318760355], (Salii, 2019)[https://doi.org/10.1016/j.ejor.2018.06.003], (Salii, Sheka, 2020)[https://doi.org/10.1080/10556788.2020.1817447], which led to finally finding the optimal solutions to `ry48p.3.sop` and `kro124p.4.sop`.


## Usage
1. Compile it with `GHC`. See the template in `build.sh`.
2. The executable (call it `precanz`) processes all `.sop` files in the directory it is run in. For each one, it outputs a `.sop.anz` plaintext report where it found the input `.sop`. When it finishes, it also outputs a `.csv` processing summary, with each `.sop`'s _width_, _density_, etc.
3. Expect it to take several minutes for the whole TSPLIB/SOPLIB06.

## Acknowledgements 
The `MaxBipartiteMatching` package (`Matcher.lhs` and `MaxBipartiteMatching.lhs`) is by Stefan Klinger [https://stefan-klinger.de/](https://stefan-klinger.de/), and was available under GNU AGPL v.3. I include it unpacked to simplify the compilation of `precanz`. 

The implementation of _transitive closure_ is based on the functional version of the Roy–Warshall algorithm, see the details in (Berghammer, Fisher, 2015)[https://doi.org/10.1016/j.jlamp.2014.08.003].

I liked working with _transitive closure_ and _reduction_ in **relational** statement, where it's all multiplying matrices and the like. See e.g. _Schmidt, G. and Stroehlein, T. 1993 “Relations and Graph Discrete Mathematics for Computer Scientists”_

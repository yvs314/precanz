# Precedence Constraints Analyzer
Given a TSPLIB-compatible input file (.sop), encoding an instance of Precedence Constrained TSP (same as Sequential Ordering Problem), compute the _width_ of the partial order given by the precedence constraints, as well as its _transitive reduction_ and _density_.

I wrote it to estimate if it was feasible to solve specific instances of TSPLIB, and used it in (Salii, 2017)[https://www.researchgate.net/publication/318760355], (Salii, 2019)[https://doi.org/10.1016/j.ejor.2018.06.003], (Salii, Sheka, 2020)[https://doi.org/10.1080/10556788.2020.1817447], which led to finally finding the optimal solutions to `ry48p.3.sop` and `kro124p.4.sop`.

### Auxiliary script `/aux/T2S.jl`

The Julia script `/aux/T2S.jl` transmutes SOPLIB06 files into TSPLIB format, with one caveat: it does not apply _transitive closure_ to precedence constraints, even though it is mandated by the TSPLIB standard. 

## Usage
1. Compile it with `GHC`. See the template in `build.sh`.
2. The executable (call it `precanz`) processes all `.sop` files in the directory it is run in. For each one, it outputs a `.sop.anz` plaintext report where it found the input `.sop`. When it finishes, it also outputs a `.csv` processing summary, with each `.sop`'s _width_, _density_, etc.
3. Expect it to take several minutes for the whole TSPLIB/SOPLIB06.

### Usage auxiliary `/aux/T2S.jl`
1. Run it from `/data` e.g. by `julia ../aux/T2S.jl`
2. It processes all the .sop files from '/data/SOPLIB', and puts the resulting TSPLIB-like .sop in `/data/t-SOPLIB`, 

## Acknowledgements 
The venerable TSPLIB is by George Reinelt, with SOP instances contributed by Norbert Ascheuer and Laureano Escudero. The authoritative link is (http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/); in this repo, the SOP instances are in `/data/TSPLIB-SOP`.

SOPLIB06 is by Roberto Montemanni (roberto@idsia.ch).

The `MaxBipartiteMatching` package (`Matcher.lhs` and `MaxBipartiteMatching.lhs`) is by Stefan Klinger [https://stefan-klinger.de/](https://stefan-klinger.de/), and was available under GNU AGPL v.3. I include it unpacked to simplify the compilation of `precanz`. 

The implementation of _transitive closure_ is based on the functional version of the Roy–Warshall algorithm, see the details in (Berghammer, Fisher, 2015)[https://doi.org/10.1016/j.jlamp.2014.08.003].

I liked working with _transitive closure_ and _reduction_ in **relational** statement, where it's all multiplying matrices and the like. See e.g. _Schmidt, G. and Stroehlein, T. 1993 “Relations and Graph Discrete Mathematics for Computer Scientists”_

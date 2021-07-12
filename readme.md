# Precedence Constraints Analyzer
Given a TSPLIB-compatible input file (.sop), encoding an instance of Precedence Constrained TSP (same as Sequential Ordering Problem), compute the **width** of the **partial order** given by the _precedence constraints_, as well as its **transitive reduction** and **density**.

I wrote it to estimate if it was feasible to solve specific SOP instances of TSPLIB by a flavor of _dynamic programming_, and used these estimates in [(Salii, 2017)](https://www.researchgate.net/publication/318760355), [(Salii, 2019)](https://doi.org/10.1016/j.ejor.2018.06.003), and [(Salii, Sheka, 2020)](https://doi.org/10.1080/10556788.2020.1817447), which led to finally finding the optimal solutions to `ry48p.3.sop` and `kro124p.4.sop`.

### Auxiliary script `/aux/T2S.jl`

The Julia script `/aux/T2S.jl` transmutes SOPLIB06 files into TSPLIB format, with one caveat: it does not apply _transitive closure_ to precedence constraints, even though it is mandated by the TSPLIB standard. 

## Usage
1. Compile it with `GHC`. See the template in `build.sh`.
2. The executable `precanz` processes all `.sop` files in the directory it is run from. For each `.sop` file, it outputs a `.sop.anz` plaintext report. When it finishes, it also outputs a `precAnz.csv` processing summary, with each `.sop`'s _width_, _density_, etc.
3. Expect it to take several minutes for the whole TSPLIB/SOPLIB06.

For convenience, I include the processed TSPLIB and SOPLIB in `/data/TSPLIB-SOP-anz` and `/data/t-SOPLIB-anz`, respectively. 

### Usage, auxiliary `/aux/T2S.jl`
1. Run it from `/data` e.g. by `julia ../aux/T2S.jl`
2. It processes all the .sop files from '/data/SOPLIB', and puts the resulting TSPLIB-like .sop in `/data/t-SOPLIB`, 


## Acknowledgements 
I wrote this analyzer in 2016–2017 when I was a Junior Researcher with _Krasovskii Institute of Mathematics and Mechanics_ UB RAS in Yekaterinburg, Russia.

The `MaxBipartiteMatching` package (`Matcher.lhs` and `MaxBipartiteMatching.lhs`) is by Stefan Klinger [https://github.com/s5k6/maxBipartiteMatching](https://github.com/s5k6/maxBipartiteMatching), and was available under GNU AGPL v.3. I include it unpacked to simplify the compilation of `precanz`. 

The venerable [TSPLIB](https://doi.org/10.1287/ijoc.3.4.376) is by George Reinelt, with SOP instances contributed by Norbert Ascheuer and Laureano Escudero [(Ascheuer, Junger, Reinelt, 2000)](https://doi.org/10.1023/A:1008779125567). In this repo, the SOP instances are in `/data/TSPLIB-SOP`. The authoritative link is (http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/).

SOPLIB06 is by [Roberto Montemanni](https://orcid.org/0000-0002-0229-0465); in this repo, these are located in `/data/SOPLIB`, and when transmuted to TSPLIB-like format, in `/data/t-SOPLIB`.


The implementation of _transitive closure_ is based on the functional version of the Roy–Warshall algorithm, see the details in [(Berghammer, Fisher, 2015)](https://doi.org/10.1016/j.jlamp.2014.08.003).

I liked working with _transitive closure_ and _reduction_ in **relational** statement, where it's all multiplying matrices and the like. See e.g. _Schmidt, G. and Stroehlein, T. 1993 “Relations and Graph Discrete Mathematics for Computer Scientists”_



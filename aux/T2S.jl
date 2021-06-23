""" 
Yaroslav Salii, 2020

This script reads every .sop (SOPLIB06) from `./sopDir`,
and transforms them into .sop (TSPLIB) in `./outDir`
CAVEAT: this script does not check if precedence constraints are *transitively closed*, 
although is mandated by TSPLIB standard. SOPLIB06 is *not* transitively closed.
"""

using DelimitedFiles #just to read the ingress SOPLIB06 .sop files

sopDir = "SOPLIB"
outDir = "t-SOPLIB"
outPath = joinpath(".",outDir)
rm(outPath, recursive=true, force=true) #trash the previous output
mkpath(outPath) #create the output directory


#get all filenames from ./$sopDir that end in "sop"
sops = filter(s -> s[end-2:end]=="sop", readdir(sopDir))
#now just stick all the working code into the *for* loop. Feels so dirty!
for ifName in sops
    ifPath = joinpath(".",sopDir,ifName) 
    rawA = readdlm(ifPath,'\t') #SOPLIB06 are CSVs with '\t';
    #Transform: first drop the empty column (the last one, from dangling delimiters)
    A = map(Int,rawA[1:end,1:end-1]); 
    #Transform: add the missing TSPLIB-SOP quirk, depot to terminal = 1000000
    A[1,end] = 1000000
    #Transform: reduce each row by concat-space
    B=[ reduce((s, z) -> "$s $z",map(string,A[row,1:end])) for row=1:size(A,1)]
    #Transform: intersperse the rows with newlines '\n'
    C = reduce( (s,z) -> s*'\n'*z,B)
    
    #Prep. output: collect TSPLIB-like stats in a string
    comment = "Part of SOPLIB06 by Roberto Montemanni"
    dim = size(A,1) #how many vertices, including depot and terminal
    #careful: make sure $ifName is ONLY file name, no directories, no paths
    #unimplemented quirk: three spaces' indent before each row
    legend ="""
    NAME: $ifName
    TYPE: SOP
    DIMENSION: $dim
    EDGE_WEIGHT_TYPE: EXPLICIT
    EDGE_WEIGHT_FORMAT: FULL_MATRIX 
    EDGE_WEIGHT_SECTION
    $dim
    $C
    EOF
    """
    
    # Writing prep
    ofName = ifName
    outFilePath = joinpath(outPath,ofName)
    
    # Write $legend into $outFilePath
    open(outFilePath,"w") do outFile
        write(outFile,legend)
    end
end

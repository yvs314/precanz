
rm -rf build
mkdir build
#cd build
ghc -O2 -isrc src/processSOP.hs -outputdir build/  -o build/precanz

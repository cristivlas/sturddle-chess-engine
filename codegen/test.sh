# build codegen
./make.sh

# generate test header
./codegen > test.h

# build test program
CARGS="-std=c++17 -march=native -O3 -I.. -I../libpopcnt -I../magic-bits/include -DTESTGEN"

echo clang++ $CARGS testgen.cpp -g -o testgen
clang++ $CARGS testgen.cpp ../chess.cpp -g -o testgen

# run perf test
./testgen

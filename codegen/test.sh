# build codegen
./make.sh

# generate test header
./codegen > test.h

# build test program
CC=${CC:-clang++}
CARGS="-std=c++17 -march=native -O3 -I.. -I../libpopcnt -I../magic-bits/include -DTESTGEN"

CMD="${CC} ${CARGS} testgen.cpp -g -o testgen"
echo $CMD
$CMD

# run perf test
./testgen

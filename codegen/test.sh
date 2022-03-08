./make.sh
./codegen > test.h
clang++-12 -march=native -O3 -I.. -I../libpopcnt testgen.cpp ../chess.cpp -o testgen
./testgen

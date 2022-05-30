./make.sh
./codegen > test.h
clang++ -std=c++17 -march=native -O3 -I.. -I../libpopcnt testgen.cpp ../chess.cpp -o testgen
./testgen

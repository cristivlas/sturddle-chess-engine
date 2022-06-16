CC=${CC:-clang++}
CMD="$CC $CFLAGS -std=c++17 -Wall -I.. -I../libpopcnt -I../magic-bits/include codegen.cpp ../chess.cpp -o codegen"
echo $CMD
$CMD

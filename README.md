Sturddle is a chess engine that was developed from scratch with the goal of helping the author learn about chess programming.
It was initially written in Python, but some performance-critical parts were later rewritten in C++.
The engine is called Sturddle, which refers to its hybrid architecture, and it is set to use the MTD(f) search algorithm by default,
but it can be changed to Negascout or Negamax using UCI commands.
The UCI protocol was initially implemented in Python and then converted to Cython.
An experimental native C++ implementation of the UCI protocol is also available and can be enabled by setting the UCI_NATIVE environment variable to 1,
which requires a compiler that supports C++20. 

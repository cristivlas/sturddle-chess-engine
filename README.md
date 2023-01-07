Sturddle is a chess engine that was developed from scratch to help the author learn about chess programming.
Its name, "Sturddle," refers to the hybrid nature of the engine, which combines various technologies and techniques.
The engine was initially written in Python, but many parts have since been rewritten in C++ for better performance.
It uses the MTD(f) search algorithm by default, but can be changed to Negascout or Negamax using UCI commands.
The UCI protocol was initially implemented in Python and then converted to Cython. There is also an experimental
C++ implementation of the protocol that can be enabled at compile-time by setting the NATIVE_UCI environment variable
to 1 (this requires a C++20-compatible compiler).

# Sturddle

__For the latest versions see__: https://github.com/cristivlas/sturddle-2

Sturddle is a chess engine developed to help the author learn about chess programming. Its name, "Sturddle," refers to the hybrid nature of the engine, which combines various technologies and techniques. The engine was initially written in Python, but it has since been almost entirely rewritten in C++ for improved performance. It uses the MTD(f) search algorithm by default, but can be changed to Negascout or Negamax using UCI commands. The UCI protocol was initially implemented in Python, then converted to Cython. There is also an experimental C++ implementation of the protocol that can be enabled at compile-time by setting the NATIVE_UCI environment variable to 1, but this requires a compiler that supports the C++20 standard.

The Sturddle project is designed to be a platform for experimentation. For example, there are multiple implementations of the SEE (Static Exchange Evaluation) heuristic that can be selected at compile-time, and various heuristics and features can be turned on or off at compile-time, such as NNUE (Efficiently Updated Neural Networks).

In addition to these options, the engine can also be auto-tuned by compiling it with the -DTUNING_ENABLED=1 flag and using the tuneup/gentune.py utility script to generate configurations for the chess-tuning-tools package (https://github.com/kiudee/chess-tuning-tools). This flexibility to tweak and tune various parameters and features is a key feature of the Sturddle project, allowing developers to test and compare different approaches and configurations.


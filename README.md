Sturddle is a chess engine written from scratch, with ideas taken from Wikipedia, https://www.chessprogramming.org/, talkchess.com,
other internet sources, and The Computer Chess Compendium by David Levy et al. The goal of the project is mainly autodidactic.

The engine started as a prototype written in Python, and performance-critical functionality was rewritten in C++ over time.
The name Sturddle (as in the sturddlefish hybrid) is a hint to the resulting architecture.

The default search algorithm is MTD(f); it can be changed at runtime to Negascout or Negamax via UCI commands.

The initial implementation of the UCI protocol was written in Python, then converted to Cython.
An alternative, native C++ UCI implementation can be compiled by setting UCI_NATIVE=1 in the environment (requires C++20).


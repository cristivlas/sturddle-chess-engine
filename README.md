# sturddle-chess-engine
This chess engine was written from scratch for the purpose of learning chess programming,
with ideas taken from Wikipedia, https://www.chessprogramming.org/, talkchess.com, other internet
sources, and The Computer Chess Compendium by David Levy et al., 

Some concepts may have been misunderstood, or implemented incorrectly. The code structure may appear 
convoluted or over-engineered, as I tried to separate and encapsulate heuristics and features, and be able
to turn them on and off (or tweak) at either compile-time or run-time (for experimental and self-teaching purposes).

The engine started as a prototype written in Python, with functionality gradually moved over to C++, hence
the name Sturddle (as in the hybrid Sturddlefish). 

Currently the bulk of the implementation is in C++, with "entry-points" exposed to Python via the \_\_init\_\_.pyx 
Cython "glue". The UCI protocol is written in Python (sturddle.py).

As of now, there is no endgame tables support, and time management is minimal and sketchy.

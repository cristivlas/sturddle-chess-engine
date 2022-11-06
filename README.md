# sturddle-chess-engine
Sturddle is a chess engine written from scratch, with ideas taken from Wikipedia, https://www.chessprogramming.org/, talkchess.com, other internet sources, and The Computer Chess Compendium by David Levy et al. The goal of the project is auto-didactical: to learn chess programming.

The engine started as a prototype written in Python, with functionality gradually moved over to C++, the result being a hybrid architecture (hence the name Sturddle, from the hybrid sturddlefish). 

The bulk of the code is C++, with "entry-points" exposed to Python via the \_\_init\_\_.pyx Cython "glue". The UCI protocol is implemented in Python (sturddle.py).


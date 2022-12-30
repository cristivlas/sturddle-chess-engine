#! /usr/bin/env python3
import argparse
import asyncio
import logging
import time

import chess
import chess.engine


class EngineTestScope:
    def __init__(self, args):
        self.args = args
        self.start_time = None
        self.end_time = None
        engine_command_line = [ args.engine ]
        self.engine = chess.engine.SimpleEngine.popen_uci(engine_command_line)

    def __enter__(self):
        self.start_time = time.perf_counter()
        return self

    def __exit__(self, *_):
        self.end_time = time.perf_counter()
        self.engine.quit()

    @property
    def elapsed(self):
        return self.end_time - self.start_time


'''
Test the UCI "position" command.
'''
def test_position(test):
    for _ in range(args.iterations):
        for pos in [
            'r1bqkbnr/p1pp1ppp/1pn5/4p3/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 2 4',
            '8/7p/5k2/5p2/p1p2P2/Pr1pPK2/1P1R3P/8 b - -',
            '8/8/4R3/2r3pk/6Pp/7P/1PPB1P2/1K1R4 b - g3',
            'r1bqkbnr/ppp2ppp/2np4/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq -',
            'r1bqkb1r/ppp2ppp/2n2n2/4p3/4p3/N2PB3/PPPQ1PPP/R3KBNR w KQkq -',
            'r1bqk2r/ppp2ppp/2nb1n2/4p3/4P3/N3BP2/PPPQ2PP/R3KBNR b KQkq -',
            'r3kb1r/pppbqppp/5n2/3Pp3/1nP5/N2PBN2/PP1Q1PPP/R3KB1R b KQkq -',
            'r3kb1r/pppbqppp/5n2/3Pp3/1nP5/N2PBN2/PP1Q1PPP/R3KB1R b KQkq -',
            'r3kb1r/pppbqppp/5n2/3Pp3/1nP5/N2PBN2/PP1Q1PPP/R3KB1R b KQk -',
            'r3kb1r/pppbqppp/2N2n2/3Pp3/1nP5/3PBN2/PP1Q1PPP/R3KB1R b KQkq -',
            'r3kb1r/pppbqppp/5N2/3Pp3/1nP5/3PBN2/PP1Q1PPP/R3KB1R b KQkq -',
            'r3k2r/pppbqppp/5n2/3Pp3/1nP5/N2PBN2/PP1Q1PPP/R3K2R b KQkq -',
            'r1bqk2r/ppp2p1p/5B2/4b3/4p3/N2P4/P1PQ1PPP/R3KBNR w KQkq -',
            'r1bqk2r/ppp2p1p/5B2/4b3/4p3/N2P4/P1PQ1PPP/R3KBNR b KQkq -',
        ]:
            board = chess.Board(pos)
            test.engine.protocol._position(board)
            assert board == test.engine.protocol.board


def extract_depth(info_line):
    # Split the line by space to get a list of words
    words = info_line.split()
    try:
        # Find the index of the word "depth"
        depth_index = words.index('depth')
    except:
        return 0
    # The depth value is the word at the index after "depth", so we return the word at depth_index + 1
    return int(words[depth_index + 1])


'''
Send a "go" command to the engine, optionally setting up the position.
'''
class GoCommand(chess.engine.BaseCommand[chess.engine.UciProtocol, None]):
    depth = 0

    def __init__(self, engine, **kwargs):
        super().__init__(engine)
        self.pos = kwargs.pop('fen', None)
        self.time = kwargs.pop('movetime', 0)
        self.moves = kwargs.pop('moves', [])

    def start(self, engine):
        if self.pos:
            if self.moves:
                engine.send_line(f'position fen {self.pos} moves {" ".join(self.moves)}')
            else:
                engine.send_line(f'position fen {self.pos}')
        # Do not use the opening book
        if 'stockfish' not in args.engine:
            engine.send_line('setoption name OwnBook value false')
        engine.send_line('setoption name Ponder value false')
        engine.send_line(f'go movetime {self.time}')

    def line_received(self, engine, line):
        if line.startswith('info '):
            if args.verbose:
                print(line)
            GoCommand.depth = extract_depth(line)
        else:
            self.result.set_result(line)
            self.set_finished()

'''
Test the UCI "go" command.
'''
def test_go(test):
    for pos, moves in [
        ('8/7p/5k2/5p2/p1p2P2/Pr1pPK2/1P1R3P/8 b - -', []),
        ('8/8/4R3/2r3pk/6Pp/7P/1PPB1P2/1K1R4 b - g3', []),
        ('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -',[]),
        ('r1bqkbnr/1ppn1ppp/4p3/p3N3/3Pp3/2P5/PP1N1PPP/R1BQKB1R b KQkq -', ['d7e5', 'd4e5', 'e4e3', 'f2f3', 'e3d2', 'c1d2', 'c8d7']),
    ]:
        def _go(engine):
            return GoCommand(engine, fen=pos, moves=moves, movetime=1000)

        response = test.engine.communicate(_go)
        print (f'depth {GoCommand.depth} {response}')
        assert response.startswith('bestmove '), response


def test_tricky(test):
    tests = [
        '3K4/3P2k1/8/8/8/8/2r5/5R2 w - -',  # Lucena
    ]
    for fen in tests:
        board = chess.Board(fen=fen)
        info = test.engine.analyse(board, chess.engine.Limit(time=3))
        print(f'{info["score"]}, depth={info["depth"]}')

def run_tests(args):
    for test in [
        test_position,
        test_tricky,
        test_go,
    ]:
        with EngineTestScope(args) as test_scope:
            test(test_scope)
        print(f'{test.__name__:.<25s} Elapsed time: {test_scope.elapsed:.4f} seconds')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-e', '--engine', default='./main.py')
    parser.add_argument('-i', '--iterations', type=int, default=1, help='number of iterations to run')
    parser.add_argument('-v', '--verbose', action='store_true', help='verbose logging')

    args = parser.parse_args()
    run_tests(args)

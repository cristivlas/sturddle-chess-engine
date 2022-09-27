"""
Sturddle Chess Engine (c) 2022 Cristi Vlasceanu.
-------------------------------------------------------------------------

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------

Any third-party files include in this project are subject to copyright
and licensed as stated in their respective header notes.
"""
import io

import chess
import chess.pgn
import chess.polyglot

import chess_engine as engine


def test_castling():
    tests = [
        ('kingside white', 'r1bqkbnr/ppp2ppp/2np4/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq -', 'e1g1'),
        ('queenside white', 'r1bqkb1r/ppp2ppp/2n2n2/4p3/4p3/N2PB3/PPPQ1PPP/R3KBNR w KQkq -', 'e1c1'),
        ('kingside black', 'r1bqk2r/ppp2ppp/2nb1n2/4p3/4P3/N3BP2/PPPQ2PP/R3KBNR b KQkq -', 'e8g8'),
        ('queenside black', 'r3kb1r/pppbqppp/5n2/3Pp3/1nP5/N2PBN2/PP1Q1PPP/R3KB1R b KQkq -', 'e8c8'),
    ]
    for name, epd, move in tests:
        move = chess.Move.from_uci(move)
        board = chess.Board(fen=epd)
        state = engine.BoardState(board)

        board.push(move)
        epd, castling_rights = board.epd(), board.castling_rights

        state.apply(move)
        board.reset()
        assert board.fen() == chess.STARTING_FEN

        state.copy_to_board(board)
        assert board.epd() == epd, (name, board.epd(), epd)
        assert board.castling_rights == castling_rights, (name, board.castling_rights, castling_rights)


def test_castling_moves_generation():
    tests = [
        ('kingside white', 'r1bqkbnr/ppp2ppp/2np4/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq -'),
        ('kingside white, attacked', 'r2qkbnr/ppp2ppp/2np4/1b2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq -'),
        ('kingside white, no rights', 'r1bqkbnr/ppp2ppp/2np4/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w Qkq -'),

        ('queenside white', 'r1bqkb1r/ppp2ppp/2n2n2/4p3/4p3/N2PB3/PPPQ1PPP/R3KBNR w KQkq -'),
        ('queenside white, no rights', 'r1bqkb1r/ppp2ppp/2n2n2/4p3/4p3/N2PB3/PPPQ1PPP/R3KBNR w Kkq -'),
        ('queenside white, attacked', 'r1bqkb1r/ppp2ppp/5n2/4p3/4p3/Nn1PB3/PPPQ1PPP/R3KBNR w KQkq -'),
        ('white', 'r3k2r/pppbqppp/5n2/3Pp3/1nP5/N2PBN2/PP1Q1PPP/R3K2R w KQkq -'),

        ('kingside black', 'r1bqk2r/ppp2ppp/2nb1n2/4p3/4P3/N3BP2/PPPQ2PP/R3KBNR b KQkq -'),
        ('kingside black, no rights', 'r1bqk2r/ppp2ppp/2nb1n2/4p3/4P3/N3BP2/PPPQ2PP/R3KBNR b KQq -'),

        ('queenside black', 'r3kb1r/pppbqppp/5n2/3Pp3/1nP5/N2PBN2/PP1Q1PPP/R3KB1R b KQkq -'),
        ('queenside black, no rights', 'r3kb1r/pppbqppp/5n2/3Pp3/1nP5/N2PBN2/PP1Q1PPP/R3KB1R b KQk -'),
        ('queenside black, attacked', 'r3kb1r/pppbqppp/2N2n2/3Pp3/1nP5/3PBN2/PP1Q1PPP/R3KB1R b KQkq -'),
        ('queenside black, check!', 'r3kb1r/pppbqppp/5N2/3Pp3/1nP5/3PBN2/PP1Q1PPP/R3KB1R b KQkq -'),
        ('black', 'r3k2r/pppbqppp/5n2/3Pp3/1nP5/N2PBN2/PP1Q1PPP/R3K2R b KQkq -'),

        ('white rook attacked', 'r1bqk2r/ppp2p1p/5B2/4b3/4p3/N2P4/P1PQ1PPP/R3KBNR w KQkq -'),
        ('black rook attacked', 'r1bqk2r/ppp2p1p/5B2/4b3/4p3/N2P4/P1PQ1PPP/R3KBNR b KQkq -'),
    ]

    for name, epd, in tests:
        board = chess.Board(fen=epd)
        state = engine.BoardState(board)

        py_chess_moves = list(board.generate_castling_moves())
        py_chess_moves.sort(key=lambda move: move.uci())

        my_moves = state.castling_moves()
        my_moves.sort(key=lambda move: move.uci())

        # print (name, my_moves)
        assert py_chess_moves == my_moves, (name, py_chess_moves, my_moves)
        assert chess.polyglot.zobrist_hash(board) == state.zobrist()


def test_en_passant():
    tests = [
        ('black pass', '4k3/3p4/8/4P3/8/8/8/4K3 b', 'd7d5', 0),
        ('white take', '4k3/8/8/3pP3/8/8/8/4K3 w - d6', 'e5d6', 100),
        ('white pass', '4k3/8/8/8/3p4/8/4P3/4K3 w - -', 'e2e4', 0),
        ('black take', '4k3/8/8/8/3pP3/8/8/4K3 b - e3', 'd4e3', 100),
    ]

    for name, epd, move, value in tests:
        move = chess.Move.from_uci(move)
        board = chess.Board(fen=epd)
        state = engine.BoardState(board)

        board.push(move)
        epd, ep_square = board.epd(), board.ep_square

        state.apply(move)
        board.reset()
        assert board.fen() == chess.STARTING_FEN

        state.copy_to_board(board)

        assert board.epd() == epd, (name, board.epd(), epd)
        assert board.ep_square == ep_square, (name, board.ep_square, ep_square)
        assert state.capture_value() == value, (name, value, state.capture_value())


def test_is_checkmate():
    tests = [
        ('Mate in 1', '6k1/1b2K1P1/7R/8/3B4/8/8/8 w - -'),
        ('Mate', '6kR/1b2K1P1/8/8/3B4/8/8/8 b - -'),
        ('En passant evasion', '8/8/4R3/2r3pk/6Pp/7P/1PPB1P2/1K1R4 b - g3'),
        ('No evasion', '8/8/4R3/2r3pk/6Pp/7P/1PPB1P2/1K1R4 b -'),
    ]

    for name, epd, in tests:
        board = chess.Board(fen=epd)
        state = engine.BoardState(board)

        py_chess_result = board.is_checkmate()
        my_result = state.is_checkmate()

        assert py_chess_result == my_result, (name, py_chess_result, my_result)



def test_connected_rooks():
    tests = [
        ('rooks.01', '1rb1r1k1/p1p2ppp/8/2pn4/5P2/2QB4/qNP3PP/2KRB2R w - -', (False, False)),
        ('rooks.02', '1rb1r1k1/p1p2ppp/8/2pn4/5P2/2QB4/qNP2BPP/2KR3R w - -', (True, False)),
        ('rooks.03', '1r2r1k1/p1pb1ppp/8/2pn4/5P2/2QB4/qNP2BPP/2KR3R w - -', (True, True)),
        ('rooks.04', '1r4k1/p1pb1ppp/1r6/2pn4/5P2/2QB4/qNP2BPP/2KR3R w - -', (True, True)),
        ('rooks.05', '1r4k1/pbp2ppp/1r2r3/2pn4/5P2/2QB4/qNP3PP/2K1R1BR w - -', (False, True)),
    ]

    for id, epd, expected in tests:
        board = chess.Board(fen=epd)
        state = engine.BoardState(board)

        # engine.print_board(board)
        result_w = state.has_connected_rooks(chess.WHITE)
        result_b = state.has_connected_rooks(chess.BLACK)

        assert expected[0] == result_w, (id, expected[0], result_w)
        assert expected[1] == result_b, (id, expected[1], result_b)


def test_pins():
    tests = [
        ('pin.01', '4k3/1p6/8/4q3/2p2P2/P2p4/1r5P/4RK2 b - -', True),
        ('pin.02', '4k3/1p6/8/4q3/2p2P2/P2p4/7P/4r1K1 b - -', False),
        ('pin.03', '4k3/1p6/8/4r3/2p2P2/P2p4/5K1P/4R3 b - -', True),
        ('pin.04', '6k1/8/2p5/3q1p2/8/1B6/5PP1/1R4K1 b - -', True),
        ('pin.05', '6k1/8/2p5/3q1p2/8/8/2B2PP1/1R4K1 b - -', False),
        ('pin.06', '6k1/8/2p5/3q1p2/8/5P2/6P1/3rQ1K1 w - -', True),
    ]
    for id, epd, expected in tests:
        board = chess.Board(fen=epd)
        state = engine.BoardState(board)

        # pin with threat to capture
        result = state.is_pinned(state.turn)
        assert expected == result, f'{id}: expected={expected}, got {result}'


def test_static_exchanges():
    tests = [
        ('capt.01', '3r4/1pk2p1N/p1n1p3/4Pq2/2Pp1b1Q/8/PP4PP/R1K1R3 w - - 0 2', 'e5', chess.BLACK, 100),
        ('capt.02', 'r2r2k1/1pp2ppp/p2q1b2/3pN3/2PP4/PP1Q3P/5PP1/R3R1K1 b - - 0 22', 'e5', chess.BLACK, 0),
        ('capt.03', 'r4rk1/1ppnbppp/p2q4/3pNb2/3P4/PP5P/2PNBPP1/R2QK2R w KQ - 5 14', 'e5', chess.BLACK, 100),
        ('capt.04', 'r4rk1/1pp2ppp/p2q1b2/4N3/2QP4/PP5P/5PP1/R3R1K1 w - - 1 24', 'f7', chess.WHITE, 0),
        # ('capt.05', 'r4rk1/2p2ppp/p2q1b2/1p2N3/1P1P1R2/PQ3P1P/6P1/3R2K1 b - - 6 31', 'e5', chess.BLACK, 80),
        # ('capt.06', 'r3r1k1/2p2ppp/p2q1b2/1p2N3/1P1PR3/P4P1P/2Q3P1/3R2K1 w - - 7 32', 'e5', chess.BLACK, 80),
        ('capt.07', 'rnb1kbnr/ppp1pppp/8/8/4qP2/2B5/PPP1N1PP/RN1QKB1R w KQkq - 3 8', 'f4', chess.BLACK, 0),
        ('capt.08', '5r2/3k1r2/1pqPp3/p2pP2p/2p2PpP/8/PPPQ1P2/1K1R3R w - - 1 36', 'f4', chess.BLACK, 100),
        # ('capt.09', '2bqkbnr/1ppppppp/3r4/p3n3/P2P4/1P5P/1BP1PPP1/RN1QKBNR w KQk - 1 6', 'e5', chess.WHITE, 0),
        ('capt.10', 'r1bqk2r/pp1p1pbp/6p1/4p3/2PnP3/2N1B3/PP3PPP/R2QKB1R w KQkq - 0 10', 'd4', chess.WHITE, 0),
        ('capt.11', '1r1q1rk1/p3bBpp/2Q5/8/3Pb3/2n1BN2/P4PPP/R4RK1 b - - 0 18', 'c6', chess.BLACK, 0),
        # ('capt.12', '2rq1rk1/1p2b1pp/p1b1Np2/2n1P1B1/3p1PPP/4Q3/PPP5/1K1R1B1R w - - 0 21', 'e3', chess.BLACK, 0),
        ('capt.13', 'r4rk1/ppp2ppp/5n2/2bPn3/4K3/2NP4/PPPBB1PP/R6R w - - 3 3', 'e5', chess.WHITE, 325),
        ('capt.14', '2r3k1/p5p1/4p3/1p1bP3/2pb2Q1/5N2/1q3P1P/3R1RK1 b - - 3 32', 'd4', chess.WHITE, 325),

        #('capt.15', 'rqr3k1/p4p1p/5Qp1/2b5/2N5/2Pn2NP/P2B1PP1/2R2RK1 w - - 0 24', 'g3', chess.BLACK, 325),
        #depends on SEE_PIN_AWARENESS_DEPTH in common.h
        ('capt.15', 'rqr3k1/p4p1p/5Qp1/2b5/2N5/2Pn2NP/P2B1PP1/2R2RK1 w - - 0 24', 'g3', chess.BLACK, 0),

        ('capt.16', '5r1k/pp4pp/2p5/2b1q3/4P3/1PB1p3/P3Q1PP/3N2K1 w - -', 'e5', chess.WHITE, 975),
        ('capt.17', '5r1k/pp4pp/2p5/2b1P3/4P3/1PB1p3/P3Q1PP/3N1qK1 w - -', 'f1', chess.WHITE, 0),
    ]

    for id, fen, square_name, color, expected in tests:
        board = chess.Board(fen=fen)
        state = engine.BoardState(board)

        square = chess.parse_square(square_name)
        value = engine.eval_static_exchanges(board, color, square)

        assert value == expected, (id, f'expected={expected} got={value}', value)


def test_zobrist():
    tests = [
        ('z.01', '3r4/1pk2p1N/p1n1p3/4Pq2/2Pp1b1Q/8/PP4PP/R1K1R3 w - -'),
        ('z.02', 'r2r2k1/1pp2ppp/p2q1b2/3pN3/2PP4/PP1Q3P/5PP1/R3R1K1 b - -'),
        ('z.03', 'r4rk1/1ppnbppp/p2q4/3pNb2/3P4/PP5P/2PNBPP1/R2QK2R w -'),
        ('z.04', 'r4rk1/1pp2ppp/p2q1b2/4N3/2QP4/PP5P/5PP1/R3R1K1 w - -'),
        ('z.05', 'rnb1kbnr/ppp1pppp/8/8/4qP2/2B5/PPP1N1PP/RN1QKB1R w KQkq -'),
        ('z.06', 'r4rk1/1ppnbppp/p2q4/3pNb2/3P4/PP5P/2PNBPP1/R2QK2R w K -'),
        ('z.07', 'r4rk1/1ppnbppp/p2q4/3pNb2/3P4/PP5P/2PNBPP1/R2QK2R w KQ -'),
        ('z.08', 'r4rk1/1ppnbppp/p2q4/3pNb2/3P4/PP5P/2PNBPP1/R2QK2R b kq -'),
        ('z.ep', '8/8/4R3/2r3pk/6Pp/7P/1PPB1P2/1K1R4 b - g3'),
    ]
    for id, fen in tests:
        board = chess.Board(fen=fen)
        state = engine.BoardState(board)
        z1 = chess.polyglot.zobrist_hash(board)
        z2 = state.zobrist()

        assert z1 == z2, (id, z1, z2)


def test_forks():
    tests = [
        ('fork.01', 'k6r/ppp5/2b3p1/5r1p/3Q4/2PPq2N/P3PR1P/1R4K1 b - -', chess.BLACK, False),
        ('fork.02', 'k5r1/ppp1N3/2b3p1/5r1p/3Q4/2PPq3/P3PR1P/1R4K1 w - -', chess.WHITE, True),
        ('fork.03', '6k1/ppp1N3/2b3p1/5q1p/3Q4/2PP4/P3PR1P/1R4K1 b - -', chess.BLACK, False),
        ('fork.04', '6k1/ppp1N3/2b3p1/5q1p/3Q4/2PP4/P3PR1P/1R4K1 b - -', chess.WHITE, True),
        ('fork.05', '6k1/ppp5/2b1B1p1/5q1p/3Q4/2PP4/P3PR1P/1R4K1 b - -', chess.WHITE, True),
        ('fork.06', '1k1r2n1/ppp5/2b1B1p1/5q1p/2Q5/2PP4/P3PR1P/1R4K1 b - -', chess.WHITE, True),
        ('fork.07', '3r2k1/ppp5/2b1Q1p1/5q1p/8/2PP4/P3PR1P/1R4K1 b - -', chess.WHITE, False),
        ('fork.08', '1k1r4/ppp5/2b1n1p1/3P1q1p/8/1QP5/P4P1P/1R2RK2 b - -', chess.WHITE, True),
        ('fork.09', '1k1r4/ppp5/2b1n1p1/3P1q1p/8/1QP5/P4P1P/1R2RK2 b - -', chess.BLACK, False),
        ('fork.10', '1k1r4/ppp5/2b1n1p1/3P1q1p/6n1/1QP5/P4P1P/3RRK2 b - -', chess.BLACK, False),
        ('fork.11', '1k1r4/ppp5/2b1n1p1/3P1q1p/6n1/1QP5/P4P1P/3RRK2 b - -', chess.BLACK, False),
        ('fork.12', '1k1r4/ppp5/2b1n1p1/3P1q1p/6n1/1QP5/P4P1R/3R1K2 b - -', chess.BLACK, False),
        ('fork.13', '1k1r4/ppp5/2b1n1p1/3P1q1p/6n1/1QP1N3/P4P1R/3R1K2 b - -', chess.BLACK, True),
    ]
    for id, fen, color, expected in tests:
        state = engine.BoardState(chess.Board(fen=fen))
        assert state.has_fork(color)==expected, (id, expected)


def test_mobility():
    engine.set_param('MOBILITY_PAWN', 2, True)
    engine.set_param('MOBILITY_KING', 3, True)
    engine.set_param('MOBILITY_KNIGHT', 2, True)
    engine.set_param('MOBILITY_BISHOP', 2, True)
    engine.set_param('MOBILITY_ROOK', 1, True)
    tests = [
        ('mobility.01', '3k4/8/8/8/8/8/8/3K4 w - -', 0),
        ('mobility.02', '3k4/8/8/8/8/8/3K4/8 w - -', 9),
        ('mobility.03', '3k4/3p4/8/8/8/8/3K4/8 w - -', 8),
        ('mobility.04', '3k4/3p4/2N5/8/8/8/3K4/8 b - -', 21),
        ('mobility.05', '3k4/3p4/2N5/8/4b3/8/3K4/8 b - -', -3),
        ('mobility.06', '3k4/3p4/2N5/8/4b3/b7/3K4/8 b - -', -18),
        ('mobility.07', '3k4/3p4/2N5/8/8/8/3K4/4R3 b - -', 12 + 14 + 21 - 12),
        ('mobility.08', '8/8/8/8/8/8/5k2/3K4 w - -', 9 - 18),
        ('mobility.09', '8/8/8/8/2N3b1/8/2N1Pk2/3K4 w - -', 8 * 2 + 6 * 2 + 3 * 2 - 3 * 4 - 2 * 7),
        ('mobility.10', '3r4/8/4P3/6b1/8/4R3/5k2/2K5 w - -', 3 * 3 + 2 - 3 * 4 - 6 * 2 - 11)
    ]
    for id, fen, expected in tests:
        state = engine.BoardState(chess.Board(fen=fen))
        mobility = state.mobility()
        assert mobility == expected, (id, mobility, f'expected={expected}')


def test_connected_pawns():
    tests = [
        ('connected.01', '8/P5kp/3p2p1/3P4/2P2Pb1/6P1/4p2P/3rR1K1 w - -', chess.WHITE, 5),
        ('connected.02', '8/P5kp/3p2p1/3P4/2P2Pb1/6P1/4p2P/3rR1K1 w - -', chess.BLACK, 4),
        ('connected.03', '2r1k2r/2p3p1/1p2P2p/p7/7B/P1N5/6PP/R5K1 b k -', chess.BLACK, 5),
        ('connected.04', '2r1k2r/2p3p1/1p2P2p/p7/7B/P1N5/6PP/R5K1 b k -', chess.WHITE, 2),
    ]

    for id, fen, color, expected in tests:
        state = engine.BoardState(chess.Board(fen=fen))
        count = state.count_connected_pawns(color)
        assert count == expected, (id, count, f'expected={expected}')


def test_isolated_pawns():
    tests = [
        ('isolated.01', '8/P5kp/3p2p1/3P4/2P2Pb1/6P1/4p2P/3rR1K1 w - -', chess.WHITE, 1),
        ('isolated.02', '8/P5kp/3p2p1/3P4/2P2Pb1/6P1/4p2P/3rR1K1 w - -', chess.BLACK, 0),
    ]

    for id, fen, color, expected in tests:
        state = engine.BoardState(chess.Board(fen=fen))
        count = state.count_isolated_pawns(color)
        assert count == expected, (id, count, f'expected={expected}')


def test_longest_pawn_sequence():
    tests = [
        ('seq.01', '8/P5kp/3p2p1/3P4/2P2Pb1/6P1/4p2P/3rR1K1 w - -', chess.WHITE, 3),
        ('seq.02', '8/P5kp/3p2p1/3P4/2P2Pb1/6P1/4p2P/3rR1K1 w - -', chess.BLACK, 2),
        ('seq.03', 'r1bq2rk/pp3pbp/2p1p1pQ/7P/3P4/2PB1N2/PP3PPR/2KR4 w - -', chess.WHITE, 4),
        ('seq.04', 'r1bq2rk/pp3pbp/2p1p1pQ/7P/3P4/2PB1N2/PP3PPR/2KR4 w - -', chess.BLACK, 4),
        ('seq.05',  '8/7p/5k2/5p2/p1p2P2/Pr1pPK2/1P1R3P/8 b - -', chess.WHITE, 2),
        ('seq.06',  '8/7p/5k2/5p2/p1p2P2/Pr1pPK2/1P1R3P/8 b - -', chess.BLACK, 2),
        ('seq.07',  '2b5/1r6/2kBp1p1/p2pP1P1/2pP4/1pP3K1/1R3P2/8 b - -', chess.BLACK, 5),
        ('seq.08',  '2b5/1r6/2kBp1p1/p2pP1P1/2pP4/1pP3K1/1R3P2/8 b - -', chess.WHITE, 5),
    ]

    for id, fen, color, expected in tests:
        board = chess.Board(fen=fen)
        state = engine.BoardState(board)
        count = state.longest_pawn_sequence(board.occupied_co[color])
        assert count == expected, (id, count, f'expected={expected}')


def test_repetition():
    game = chess.pgn.read_game(io.StringIO('''
[FEN "4k3/4p3/8/8/8/3B4/4P3/4K3 w - -"]

1. Bg6+ Kd7 2. Bd3 Ke8 3. Bg6+ Kd7 4. Bd3'''))
    board = game.board()
    for move in game.mainline_moves():
        board.push(move)

    node = engine.NodeContext(board)
    assert node.is_repeated()


"""
 wking=1, wqueen=2, wrook=3, wbishop= 4, wknight= 5, wpawn= 6,
 bking=7, bqueen=8, brook=9, bbishop=10, bknight=11, bpawn=12,
"""
def test_nnue_piece_codes():
    assert engine.nnue_piece(chess.KING, chess.WHITE) == 1
    assert engine.nnue_piece(chess.QUEEN, chess.WHITE) == 2
    assert engine.nnue_piece(chess.ROOK, chess.WHITE) == 3
    assert engine.nnue_piece(chess.BISHOP, chess.WHITE) == 4
    assert engine.nnue_piece(chess.KNIGHT, chess.WHITE) == 5
    assert engine.nnue_piece(chess.PAWN, chess.WHITE) == 6
    assert engine.nnue_piece(chess.KING, chess.BLACK) == 7
    assert engine.nnue_piece(chess.QUEEN, chess.BLACK) == 8
    assert engine.nnue_piece(chess.ROOK, chess.BLACK) == 9
    assert engine.nnue_piece(chess.BISHOP, chess.BLACK) == 10
    assert engine.nnue_piece(chess.KNIGHT, chess.BLACK) == 11
    assert engine.nnue_piece(chess.PAWN, chess.BLACK) == 12


def test_nnue_eval():
    tests = [
        chess.STARTING_FEN,
        '3r4/1pk2p1N/p1n1p3/4Pq2/2Pp1b1Q/8/PP4PP/R1K1R3 w - - 0 2',
        'r2r2k1/1pp2ppp/p2q1b2/3pN3/2PP4/PP1Q3P/5PP1/R3R1K1 b - - 0 22',
        'r4rk1/1ppnbppp/p2q4/3pNb2/3P4/PP5P/2PNBPP1/R2QK2R w KQ - 5 14',
        'rqr3k1/p4p1p/5Qp1/2b5/2N5/2Pn2NP/P2B1PP1/2R2RK1 w - - 0 24',
        '2r3k1/p5p1/4p3/1p1bP3/2pb2Q1/5N2/1q3P1P/3R1RK1 b - - 3 32',
        'r4rk1/ppp2ppp/5n2/2bPn3/4K3/2NP4/PPPBB1PP/R6R w - - 3 3',
        '1r1q1rk1/p3bBpp/2Q5/8/3Pb3/2n1BN2/P4PPP/R4RK1 b - - 0 18',
    ]
    for fen in tests:
        eval = engine.nnue_eval_fen(fen)
        assert eval == engine.nnue_eval_board(chess.Board(fen=fen)), (fen, eval)


def test_incremental_updates():
    tests = [
        'r3k2r/pp3ppp/1np5/5PN1/3PB3/2K1P2P/PP3q2/R2Q3R b kq -',
        'r3r1k1/pp1q1pp1/4b1p1/3p2B1/3Q1R2/8/PPP3PP/4R1K1 w - -',
        'r4rk1/1bR1bppp/4pn2/1p2N3/1P6/P3P3/4BPPP/3R2K1 b - -',
        'r3r1k1/5p2/pQ1b2pB/1p6/4p3/6P1/Pq2BP1P/2R3K1 b - -',
        '8/3b2kp/4p1p1/pr1n4/N1N4P/1P4P1/1K3P2/3R4 w - -',
        '1br2rk1/1pqb1ppp/p3pn2/8/1P6/P1N1PN1P/1B3PP1/1QRR2K1 w - -',
        '5rk1/2p4p/2p4r/3P4/4p1b1/1Q2NqPp/PP3P1K/R4R2 b - -',
        'r1q2rk1/p3bppb/3p1n1p/2nPp3/1p2P1P1/6NP/PP2QPB1/R1BNK2R b KQ -',
        'rr4k1/p1pq2pp/Q1n1pn2/2bpp3/4P3/2PP1NN1/PP3PPP/R1B1K2R b KQ -',
        '2r1r2k/1q3ppp/p2Rp3/2p1P3/6QB/p3P3/bP3PPP/3R2K1 w - -',
        'r1bqk2r/pp3ppp/5n2/8/1b1npB2/2N5/PP1Q2PP/1K2RBNR w kq -',
        'r1b1r1k1/pp1nqp2/2p1p1pp/8/4N3/P1Q1P3/1P3PPP/1BRR2K1 w - -',
        '1r3r1k/3p4/1p1Nn1R1/4Pp1q/pP3P1p/P7/5Q1P/6RK w - -',
        '2kr4/ppp3Pp/4RP1B/2r5/5P2/1P6/P2p4/3K4 w - -',
        'r1b1kbr1/pp3p1p/5qp1/4p3/1P2P3/P1N3P1/5P1P/R2QKB1R w KQq -',
        '1nbq1r1k/3rbp1p/p1p1pp1Q/1p6/P1pPN3/5NP1/1P2PPBP/R4RK1 w - -',
        '3r1rk1/p3qp1p/2bb2p1/2p5/3P4/1P6/PBQN1PPP/2R2RK1 b - -',
        '3n2nr/4Pqpp/2k5/8/8/8/2B3PP/6K1 w - -',
    ]
    for fen in tests:
        engine.test_incremental_updates(fen)

test_castling()
test_castling_moves_generation()
test_connected_rooks()
test_en_passant()

test_is_checkmate()
test_pins()
test_static_exchanges()
test_zobrist()
test_forks()

#This test requires compiling with MOBILITY_TUNING_ENABLED
if engine.MOBILITY_TUNING:
    test_mobility()

test_connected_pawns()
test_isolated_pawns()
test_longest_pawn_sequence()

test_repetition()

test_nnue_piece_codes()
test_nnue_eval()
test_incremental_updates()
print ('All tests passed')

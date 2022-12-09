# distutils: language = c++
# cython: language_level = 3
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

-------------------------------------------------------------------------
C++ / python-chess hybrid chess engine.

https://en.wikipedia.org/wiki/Sturddlefish

The sturddlefish is a hybrid of the paddlefish (Polyodon spathula)
and the sturgeon (Acipenser gueldenstaedtii), accidentally created
by researchers in 2019 and announced in 2020.
"""
from cpython.ref cimport PyObject
from cython.operator cimport address, dereference as deref

from libcpp cimport bool
from libcpp.map cimport map
from libcpp.memory cimport unique_ptr
from libcpp.string cimport string
from libcpp.vector cimport vector
from libc cimport stdint

from datetime import datetime

import chess
import chess.pgn
import chess.polyglot
import chess.syzygy
import logging
import platform
import os
import sys
import time

ctypedef int score_t
ctypedef stdint.uint64_t Bitboard
ctypedef stdint.int64_t int64_t
ctypedef stdint.uint64_t uint64_t


# ostream placeholder
cdef extern from '<iostream>' namespace 'std':
    cdef cppclass ostream:
        pass

    cdef ostream clog

cdef int[2] SIGN = [-1, 1]


"""
Print chessboard using unicode symbols, with rank / file names.
"""
def print_board(board):
    for i, row in enumerate(board.unicode(empty_square='.').split('\n')):
        print(f'{8-i} {row}')
    print('  a b c d e f g h\n')


cdef extern from 'common.h':
    score_t SCORE_MAX
    score_t SCORE_MIN
    const bool MOBILITY_TUNING_ENABLED
    const bool USE_NNUE
    string timestamp() nogil


MOBILITY_TUNING = MOBILITY_TUNING_ENABLED


# ---------------------------------------------------------------------
# chess.h
# ---------------------------------------------------------------------
cdef extern from 'chess.h' namespace 'chess':
    cdef enum Color:
        BLACK = 0,
        WHITE = 1,

    cdef enum PieceType:
        NONE,
        PAWN,
        KNIGHT,
        BISHOP,
        ROOK,
        QUEEN,
        KING

    cdef enum Square:
        A1, B1, C1, D1, E1, F1, G1, H1,
        A2, B2, C2, D2, E2, F2, G2, H2,
        A3, B3, C3, D3, E3, F3, G3, H3,
        A4, B4, C4, D4, E4, F4, G4, H4,
        A5, B5, C5, D5, E5, F5, G5, H5,
        A6, B6, C6, D6, E6, F6, G6, H6,
        A7, B7, C7, D7, E7, F7, G7, H7,
        A8, B8, C8, D8, E8, F8, G8, H8,
        UNDEFINED = -1

    cdef Bitboard BB_ALL


    cdef cppclass BaseMove:
        BaseMove()
        BaseMove(Square from_square, Square to_square, PieceType promotion_type)

        Square      from_square() const
        Square      to_square() const
        PieceType   promotion() const
        bool        is_none() const
        string      uci() const

    cdef cppclass Move(BaseMove):
        Move()
        Move(const BaseMove&)

    ctypedef vector[BaseMove] PV


    cdef cppclass Position:
        Bitboard black, white, pawns, knights, bishops, rooks, queens, kings
        PieceType _piece_types[64]

        Bitboard occupied() const
        Bitboard pin_mask(int color, int square) const

        score_t eval_simple() const
        score_t eval_mobility() const
        PieceType _piece_type_at(Square) const
        PieceType piece_type_at(Square) const


    cdef cppclass BoardPosition(Position):
        pass


    cdef cppclass State(BoardPosition):
        State()
        Bitboard    castling_rights
        Square      en_passant_square
        Color       turn
        score_t     simple_score

        const int capture_value
        const PieceType promotion

        void    apply_move(const BaseMove&)

        int     count_connected_pawns(Color, Bitboard) const
        int     count_isolated_pawns(Color, Bitboard) const

        bool    equals(const State&) const
        score_t eval() const
        score_t eval_incremental(const BaseMove&) const
        size_t  hash() const
        void    rehash()

        bool    has_connected_rooks(int) const
        bool    has_fork(Color) const

        bool    is_castling(const BaseMove&) const
        bool    is_check() const
        bool    is_checkmate() const
        bool    is_endgame() const
        bool    is_en_passant(const BaseMove&) const
        bool    is_pinned(Color) const

        int     longest_pawn_sequence(Bitboard) const

        Bitboard checkers_mask(Color) const

        void generate_castling_moves(vector[Move]& moves) const
        void generate_moves(vector[Move]&, vector[Move]&) const

        vector[Move]& generate_pseudo_legal_moves(vector[Move]&, Bitboard, Bitboard) const
        size_t make_pseudo_legal_moves(vector[Move]&) const


    cdef score_t estimate_static_exchanges(const State&, Color, int, PieceType)
    cdef void zobrist_update(const State&, const BaseMove&, State&)

    bool parse_fen[T](const string&, T&)


cdef extern from 'zobrist.h' namespace 'chess':
    cdef size_t zobrist_hash(const State&)


assert BLACK == chess.BLACK
assert WHITE == chess.WHITE


""" Convert C++ move to Py """
cdef py_move(const BaseMove& m):
    if not m.is_none():
        return chess.Move(m.from_square(), m.to_square(), m.promotion() if m.promotion() else None)


""" Convert Py move object to C++ """
cdef BaseMove cxx_move(move: chess.Move):
    cdef BaseMove m = BaseMove(
        move.from_square,
        move.to_square,
        move.promotion if move.promotion else PieceType.NONE
    )
    return m


cdef class BoardState:
    cdef State _state


    def __cinit__(self):
        self._state = State()


    def __init__(self, board = None):
        if board:
            self.set_from_board(board)


    def __eq__(self, other: BoardState):
        return self._state.equals(other._state)


    def __hash__(self):
        return self._state.hash()


    cdef set_from_board(self, b: chess.Board):
        #
        # set position bitboards
        #
        self._state.white = b.occupied_co[WHITE]
        self._state.black = b.occupied_co[BLACK]
        self._state.pawns = b.pawns
        self._state.knights = b.knights
        self._state.bishops = b.bishops
        self._state.rooks = b.rooks
        self._state.queens = b.queens
        self._state.kings = b.kings

        self._state.castling_rights = b.clean_castling_rights()

        if b.ep_square == None:
            self._state.en_passant_square = UNDEFINED
        else:
            self._state.en_passant_square = b.ep_square

        self._state.turn = b.turn
        for square in range(0, 64):
            self._state._piece_types[square] = self._state._piece_type_at(chess.Square(square))

        self._state.rehash()
        self._state.simple_score = self._state.eval_simple()


    cpdef void apply(self, move: chess.Move):
        cdef BaseMove m = cxx_move(move)
        cdef State prev = self._state
        self._state.apply_move(m)
        zobrist_update(prev, m, self._state)


    cpdef capture_value(self):
        return self._state.capture_value


    cpdef PieceType promotion(self):
        return self._state.promotion


    cpdef copy_to_board(self, b: chess.Board):
        b.occupied_co[WHITE] = self._state.white
        b.occupied_co[BLACK] = self._state.black
        b.occupied = self._state.occupied()
        b.pawns = self._state.pawns
        b.knights = self._state.knights
        b.bishops = self._state.bishops
        b.rooks = self._state.rooks
        b.queens = self._state.queens
        b.kings = self._state.kings
        b.castling_rights = self._state.castling_rights

        if self._state.en_passant_square != UNDEFINED:
            b.ep_square = self._state.en_passant_square
        else:
            b.ep_square = None

        b.turn = self._state.turn


    cpdef Bitboard checkers_mask(self):
        return self._state.checkers_mask(self._state.turn)


    cpdef int count_connected_pawns(self, Color color, Bitboard mask = BB_ALL):
        return self._state.count_connected_pawns(color, mask)


    cpdef int count_isolated_pawns(self, Color color, Bitboard mask = BB_ALL):
        return self._state.count_isolated_pawns(color, mask)


    cpdef bool has_connected_rooks(self, color):
        return self._state.has_connected_rooks(WHITE if color else BLACK)


    cpdef bool is_check(self):
        return self._state.is_check()


    cpdef bool is_checkmate(self):
        return self._state.is_checkmate()


    cpdef bool is_endgame(self):
        return self._state.is_endgame()


    cpdef bool is_pinned(self, Color color):
        return self._state.is_pinned(color)


    cpdef has_fork(self, Color color):
        return self._state.has_fork(color)


    cpdef int longest_pawn_sequence(self, Bitboard mask):
        return self._state.longest_pawn_sequence(mask)


    cpdef int mobility(self):
        return self._state.eval_mobility()


    cpdef Bitboard pin_mask(self, Color color, Square square):
        return self._state.pin_mask(color, square)


    @property
    def score(self):
        return self._state.simple_score


    @property
    def turn(self):
        return self._state.turn


    """ Generate castling moves (for testing) """
    def castling_moves(self):
        cdef vector[Move] moves
        self._state.generate_castling_moves(moves)
        return [py_move(m) for m in moves]


    cpdef size_t zobrist(self):
        return zobrist_hash(self._state)


    cpdef int nnue(self):
        return NNUE.eval(self._state)


# ---------------------------------------------------------------------
# context.h
# ---------------------------------------------------------------------
cdef extern from 'context.h' nogil:
    const char* const NNUE_EVAL_FILE

    cdef void run_uci_loop(const char* name, const char* version, bool) except*
    #
    # Get/set engine params via Python
    #
    cdef cppclass Param:
        int val
        int min_val
        int max_val
        string group

    void _set_param(string, int, bool) except+

    map[string, int] _get_params() except+
    map[string, Param] _get_param_info() except+


    cdef cppclass NNUE:
        @staticmethod
        int piece(PieceType, Color)

        @staticmethod
        int eval(const BoardPosition&)

        @staticmethod
        int eval_fen(const string& fen)

        @staticmethod
        bool init(const string& data_dir, const string& eval_file)

        @staticmethod
        void log_init_message()


NNUE_FILE = NNUE_EVAL_FILE.decode()
#
# Export nnue functions for testing.
#
def nnue_eval_board(board: chess.Board):
    return BoardState(board).nnue()

def nnue_eval_fen(fen):
    return NNUE.eval_fen(fen.encode())

cpdef int nnue_piece(PieceType pt, Color c):
    return NNUE.piece(pt, c)


cdef extern from 'search.h':
    score_t CHECKMATE


SCORE_CHECKMATE = CHECKMATE

SCORE_MATE_HIGH = CHECKMATE - 1000
SCORE_MATE_LOW  = -SCORE_MATE_HIGH


cdef extern from 'context.h' namespace 'search':
    ctypedef enum Algorithm:
        NEGAMAX 'search::Algorithm::NEGAMAX'
        NEGASCOUT 'search::Algorithm::NEGASCOUT'
        MTDF 'search::Algorithm::MTDF'

    cdef cppclass History:
        void    emplace(const State&) except*
        int     _fifty

    cdef cppclass IterationInfo:
        score_t score
        size_t nodes
        double knps
        int milliseconds


    ctypedef unique_ptr[History] HistoryPtr


    cdef cppclass Context:
        Algorithm       _algorithm
        const State*    _state
        const int       _ply
        int             _max_depth
        const score_t   _alpha
        const score_t   _beta
        const score_t   _score
        Context*        _parent
        HistoryPtr      _history
        Move            _move
        BaseMove        _prev
        BaseMove        _best_move
        PyObject*       _engine

        bool            (*_book_init)(const string&)
        BaseMove        (*_book_lookup)(const State&, bool)
        string          (*_epd)(const State&)
        void            (*_log_message)(int, const string&, bool)
        void            (*_on_iter)(PyObject*, Context*, const IterationInfo*)
        void            (*_on_move)(PyObject*, const string&, int)
        void            (*_on_next)(PyObject*, int64_t)
        string          (*_pgn)(Context*)
        void            (*_print_state)(const State&)
        void            (*_report)(PyObject*, vector[Context*]&)
        bool            (*_tb_probe_wdl)(const State&, int*)
        size_t          (*_vmem_avail)()

        int64_t         nanosleep(int) nogil

        @staticmethod
        void            cancel() nogil

        int64_t         check_time_and_update_nps()
        const Move*     first_valid_move() nogil

        @staticmethod
        void            init()
        bool            is_repeated() const
        int             iteration() const
        int             rewind(int where, bool reorder)

        @staticmethod
        void            set_time_limit_ms(int millisec) nogil
        void            set_time_info(int millisec, int moves, score_t eval)

        void            set_tt(TranspositionTable*) nogil
        TranspositionTable* get_tt() const
        const PV&       get_pv() nogil const
        Context*        next(bool, score_t, int)
        int             tid() const

        @staticmethod
        const string& syzygy_path()

        @staticmethod
        void set_syzygy_path(const string&)

        @staticmethod
        void set_tb_cardinality(int)

        @staticmethod
        int tb_cardinality()


cpdef board_from_state(state: BoardState):
    board = chess.Board()
    state.copy_to_board(board)
    return board


cdef board_from_cxx_state(const State& state):
    cdef BoardState board_state = BoardState()
    board_state._state = state
    return board_from_state(board_state)


cdef void print_state(const State& state) except* :
    print_board(board_from_cxx_state(state))


cdef void report(self, vector[Context*]& ctxts) except* :
    self.report_cb(self, [NodeContext.from_cxx_context(c) for c in ctxts])


cdef string pgn(Context* ctxt) except* :
    cdef NodeContext node = NodeContext.from_cxx_context(ctxt)
    moves = []
    while node.parent:
        if not node.move:
            break
        moves.insert(0, node.move)
        node = node.parent

    fen = node.board().epd()
    game = chess.pgn.Game()
    game.headers['FEN'] = fen
    game_node = game
    for move in moves:
        game_node = game_node.add_variation(move)

    exporter = chess.pgn.StringExporter(headers=True)
    return game.accept(exporter).encode()


cdef string epd(const State& state) except* :
    return board_from_cxx_state(state).epd().encode()


cdef set_log_level(level, setLevel):
    if setLevel and logging.getLogger().level > level:
        logging.getLogger().setLevel(level)


__log = {
    1: (logging.DEBUG, logging.debug),
    2: (logging.INFO, logging.info),
    3: (logging.WARNING, logging.warning),
    4: (logging.ERROR, logging.error),
}

cdef void log_message(int level, const string& message, bool forceLevel) except* :
    logLevel = logging.getLogger().level
    try:
        _level, _func = __log[level]
        set_log_level(_level, forceLevel)
        _func(message.decode())
    finally:
        logging.getLogger().setLevel(logLevel)


cdef size_t vmem_avail():
    try:
        import psutil
        return psutil.virtual_memory().available
    except:
        logging.exception('psutil')
        return 0


# ---------------------------------------------------------------------
# Polyglot opening book.
# ---------------------------------------------------------------------
_book = [None]


def opening_book():
    return _book[0]


def opening_book_init(filepath: str):
    if not os.path.isabs(filepath):
        filepath = os.path.join(os.path.dirname(__file__), filepath)
    _book[0] = None
    try:
        _book[0] = chess.polyglot.MemoryMappedReader(filepath)
    except FileNotFoundError as e:
        pass
    except:
        logging.exception(opening_book_init.__name__)
    logging.debug(f'{filepath}: {_book[0]}')
    return _book[0] != None


def opening_book_lookup(board: chess.Board, best_move: bool=False):
    if _book[0] is not None:
        try:
            if best_move:
                entry = _book[0].find(board)
            else:
                entry = _book[0].weighted_choice(board)
            return entry
        except IndexError:
            pass
        except:
            logging.exception(opening_book_lookup.__name__)


cdef bool book_init(const string& filepath) except* with gil:
    return opening_book_init(filepath.decode())


cdef BaseMove book_lookup(const State& state, bool best_move) except* with gil:
    board = board_from_cxx_state(state)
    entry = opening_book_lookup(board, best_move)
    if entry and entry.move:
        return cxx_move(entry.move)
    return BaseMove()


# ---------------------------------------------------------------------
# Python wrappers for C++ Context
# ---------------------------------------------------------------------
cdef class ContextValue:
    cdef Context _ctxt


cdef class NodeContext:
    '''
    Can be either a pointer into the C++ layer, or own value.
    '''
    cdef Context* _ctxt
    cdef ContextValue _value
    cdef public BoardState state


    def __cinit__(self, board: chess.Board=None):
        self._ctxt = NULL
        self._value = None

        if board:
            self.create_from(board)


    def __eq__(self, other: NodeContext):
        return self._ctxt == other._ctxt


    cdef void create_from(self, board: chess.Board):
        self._value = ContextValue()
        self._ctxt = address(self._value._ctxt)

        self._ctxt._book_init = <bool (*)(const string&)> book_init
        self._ctxt._book_lookup = <BaseMove (*)(const State&, bool)> book_lookup
        self._ctxt._epd = <string (*)(const State&)> epd
        self._ctxt._log_message = <void (*)(int, const string&, bool)> log_message
        self._ctxt._pgn = <string (*)(Context*)> pgn
        self._ctxt._print_state = <void (*)(const State&)> print_state
        self._ctxt._vmem_avail = <size_t (*)()> vmem_avail
        self._ctxt._tb_probe_wdl = <bool (*)(const State&, int*)> tb_probe_wdl

        self._ctxt._history.reset(new History())
        self.sync_to_board(board)


    @staticmethod
    cdef NodeContext from_cxx_context(Context* ctxt):
        cdef NodeContext node = NodeContext()
        if ctxt:
            node._ctxt = ctxt
            return node


    cpdef best_move(self):
        cdef const Move* move

        best = py_move(self._ctxt._best_move)
        if not best:
            move = self._ctxt.first_valid_move()
            if move:
                best = py_move(deref(move))

        return best


    def board(self):
        return board_from_cxx_state(deref(self._ctxt._state))


    cdef int max_depth(self) nogil:
        return self._ctxt._max_depth


    @property
    def move(self):
        return py_move(self._ctxt._move)


    @property
    def ply(self):
        return self._ctxt._ply


    def stats(self):
        return task_stats(deref(self._ctxt.get_tt()))


    @property
    def task_id(self):
        return self._ctxt.tid()


    @property
    def turn(self):
        return self._ctxt._state.turn


    cdef sync_to_board(self, board: chess.Board):
        '''
        Set history of played positions.
        Set internal representation of the position to match the board.
        '''
        # first, unwind the board to the starting position
        # (which may not necessarily be a brand new game!)
        b = board.copy()
        while b.move_stack:
            b.pop()

        state = BoardState(chess.Board(fen=b.fen()))

        for move in board.move_stack:
            state.apply(move)
            deref(self._ctxt._history).emplace(state._state)

        deref(self._ctxt._history)._fifty = board.halfmove_clock

        # setup initial state
        self.state = BoardState(board)
        self._ctxt._state = address(self.state._state)

        if board.move_stack:
            self._ctxt._move = Move(cxx_move(board.move_stack[-1]))


    cpdef get_pv(self):
        cdef vector[BaseMove] pv = self._ctxt.get_pv()
        return [move.uci().decode() for move in pv][1:]


    @property
    def alpha(self):
        return self._ctxt._alpha


    @property
    def beta(self):
        return self._ctxt._beta


    @property
    def score(self):
        return self._ctxt._score


    @property
    def parent(self):
        cdef Context* parent = self._ctxt._parent
        if parent != NULL:
            return NodeContext.from_cxx_context(self._ctxt._parent)


    @property
    def top(self):
        parent = self.parent
        return self if not parent else parent.top


    def is_repeated(self):
        return self._ctxt.is_repeated()


# ---------------------------------------------------------------------
# search.h
# ---------------------------------------------------------------------
cdef extern from 'search.h' namespace 'search':

    cdef cppclass TranspositionTable:
        TranspositionTable()

        const   size_t _check_nodes
        const   size_t _eval_count
        const   size_t _endgame_nodes
        const   size_t _futility_prune_count
        const   size_t _history_counters
        const   size_t _history_counters_hit
        const   size_t _hits
        const   size_t _late_move_prune_count
        const   size_t _null_move_not_ok
        const   size_t _nps
        const   size_t _null_move_cutoffs
        const   size_t _null_move_failed
        const   size_t _qsnodes
        const   size_t _reductions
        const   size_t _retry_reductions
        const   score_t _w_alpha
        const   score_t _w_beta
        const   int _eval_depth

        void    init() nogil
        size_t  nodes() nogil const

        const   vector[BaseMove]& get_pv() nogil const

        @staticmethod
        void    clear_shared_hashtable() nogil

        @staticmethod
        size_t  size() nogil const

        @staticmethod
        double  usage() nogil const

        @staticmethod
        size_t  get_hash_size()

        @staticmethod
        void    set_hash_size(size_t)


    cdef score_t negamax(Context&, TranspositionTable&) nogil except*
    cdef score_t mtdf(Context&, score_t, TranspositionTable&) nogil except*
    cdef score_t iterative(Context&, TranspositionTable&, int) nogil except*


cdef task_stats(const TranspositionTable& table):
    return {
        'eval-count': table._eval_count,
        'endgame-nodes': table._endgame_nodes,
        'futility-prune-count': table._futility_prune_count,
        'history-counters': table._history_counters,
        'history-counters-hit': table._history_counters_hit,
        'in-check-nodes': table._check_nodes,
        'late-move-prune-count': table._late_move_prune_count,
        'nodes': table.nodes(),
        'nps': table._nps,
        'null-move-cutoffs': table._null_move_cutoffs,
        'null-move-fail': table._null_move_failed,
        'null-move-not-ok': table._null_move_not_ok,
        'qs-nodes': table._qsnodes,
        'reductions': table._reductions,
        'retry-reductions': table._retry_reductions,
        'tt-hits': table._hits,
        'tt-usage': table.usage(),
    }


# ---------------------------------------------------------------------
# Search API
# ---------------------------------------------------------------------

def clear_hashtable():
    TranspositionTable.clear_shared_hashtable()


cdef class SearchAlgorithm:
    cdef TranspositionTable _table
    cdef score_t score
    cdef public iteration_cb, move_cb, node_cb, report_cb
    cdef public best_move
    cdef public NodeContext context # important to use the type here
    cdef public depth, is_cancelled, time_info

    def __init__(self, board: chess.Board, depth=100, **kwargs):
        NNUE.log_init_message()
        self.best_move = None
        self.depth = depth
        self.is_cancelled = False
        self.score = 0
        self.context = NodeContext(board)
        self.node_cb = kwargs.get('callback', None)
        self.report_cb = kwargs.get('threads_report', None)


    cdef void set_context_callbacks(self):
        if self.context:
            self.context._ctxt._engine = <PyObject*> self

            # iteration callback
            if self.iteration_cb:
                self.context._ctxt._on_iter = <void (*)(PyObject*, Context*, const IterationInfo*)> self.on_iter
            else:
                self.context._ctxt._on_iter = NULL

            # callback for reporting currmove and currmovenumber via UCI
            if self.move_cb:
                self.context._ctxt._on_move = <void (*)(PyObject*, const string&, int)> self.on_move
            else:
                self.context._ctxt._on_move = NULL

            # search callback (checks timer, calls user-defined callback)
            if self.node_cb:
                self.context._ctxt._on_next = <void (*)(PyObject*, int64_t)> self.on_next
            else:
                self.context._ctxt._on_next = NULL

            # callback for reporting SMP threads stats
            if self.report_cb:
                self.context._ctxt._report = <void (*)(PyObject*, vector[Context*]&)> report
            else:
                self.context._ctxt._report = NULL


    cpdef cancel(self):
        if self.context:
            Context.cancel()
            self.is_cancelled = True


    @property
    def window_alpha(self):
        return self._table._w_alpha


    @property
    def window_beta(self):
        return self._table._w_beta


    @property
    def current_depth(self):
        return self.context.max_depth()


    @property
    def eval_count(self):
        return self._table._eval_count


    @property
    def eval_depth(self):
        return self._table._eval_depth


    cpdef void extend_time_limit(self, int time_limit_ms):
        self.time_limit_ms = time_limit_ms
        with nogil:
            Context.set_time_limit_ms(time_limit_ms)


    cpdef get_pv(self):
        return self.context.get_pv()


    cpdef int64_t nanosleep(self, int nanosec):
        with nogil:
            return self.context._ctxt.nanosleep(nanosec)


    @property
    def tt_hits(self):
        return self._table._hits


    @property
    def tt_size(self):
        return self._table.size()


    @property
    def tt_usage(self):
        return self._table.usage()


    @property
    def nodes(self):
        return self._table.nodes()


    @property
    def nps(self):
        return self._table._nps


    cpdef stats(self):
        return task_stats(self._table)


    #
    # Callback wrappers
    #
    cdef void on_iter(self, Context* ctxt, const IterationInfo* i) except* with gil:
        self.iteration_cb(
            self,
            NodeContext.from_cxx_context(ctxt),
            deref(i).score,
            deref(i).nodes,
            deref(i).knps,
            deref(i).milliseconds)


    '''
    Support for communicating currmove and currmovenumber via UCI script.
    '''
    cdef void on_move(self, const string& move, int move_count) except* with gil:
        self.move_cb(move.decode(), move_count)


    cdef void on_next(self, int64_t milliseconds) except*:
        self.node_cb(self, milliseconds)


    cdef score_t _search(self, TranspositionTable& table):
        pass


    def search(self, board: chess.Board = None, **kwargs):
        self.best_move = None
        self.is_cancelled = False

        if board:
            self.context.create_from(board)

        self._table.init()
        self.set_context_callbacks()
        self.context._ctxt.set_tt(address(self._table))
        self.context._ctxt._max_depth = self.depth
        self.context._ctxt._prev = BaseMove()

        # optional: (time, moves) left till next time control
        self.time_info = kwargs.get('time_info', None)

        # call algorithm-specific implementation (Template Method design pattern)
        self.score = self._search(self._table)

        self.best_move = self.context.best_move()
        return (self.best_move, self.score)


cdef class IterativeDeepening(SearchAlgorithm):
    cdef Algorithm algorithm
    cdef public int time_limit_ms


    def __init__(self, algorithm: Algorithm, board: chess.Board, depth, **kwargs):
        super().__init__(board, depth, **kwargs)
        self.algorithm = algorithm
        self.time_limit_ms = kwargs.get('time_limit_ms', None) or kwargs.get('time_limit', 5) * 1000
        self.iteration_cb = kwargs.get('iteration_cb', None) or kwargs.get('on_iteration', None)
        self.move_cb = kwargs.get('on_move', None)


    cdef score_t _search(self, TranspositionTable& table):
        cdef int max_iter = self.depth + 1
        cdef score_t score = 0

        self.context._ctxt._algorithm = self.algorithm
        self.context._ctxt._max_depth = 1

        # Set the time limit (which also starts the clock).
        Context.set_time_limit_ms(self.time_limit_ms)

        # Provide additional info so the engine can do its own time management.
        if self.time_info:
            self.context._ctxt.set_time_info(self.time_info[0], self.time_info[1], self.score)

        with nogil:
            score = iterative(deref(self.context._ctxt), table, max_iter)

        return score


cdef class Negamax(SearchAlgorithm):
    cdef score_t _search(self, TranspositionTable& table):
        cdef score_t score = 0
        assert self.context.max_depth() == self.depth
        self.context._ctxt._algorithm = NEGAMAX
        with nogil:
            score = negamax(deref(self.context._ctxt), table)
        return score


cdef class Negascout(SearchAlgorithm):
    cdef score_t _search(self, TranspositionTable& table):
        cdef score_t score = 0
        assert self.context.max_depth() == self.depth
        self.context._ctxt._algorithm = NEGASCOUT
        with nogil:
            score = negamax(deref(self.context._ctxt), table)
        return score


cdef class MTDf(SearchAlgorithm):
    cdef score_t _search(self, TranspositionTable& table):
        cdef score_t score = 0
        assert self.context.max_depth() == self.depth
        self.context._ctxt._algorithm = MTDF
        with nogil:
            score = mtdf(deref(self.context._ctxt), 0, table)
        return score


cdef class MTDf_i(IterativeDeepening):
    def __init__(self, board, depth=100, **kwargs):
        super().__init__(MTDF, board, depth, **kwargs)


cdef class Negamax_i(IterativeDeepening):
    def __init__(self, board, depth=100, **kwargs):
        super().__init__(NEGAMAX, board, depth, **kwargs)


cdef class Negascout_i(IterativeDeepening):
    def __init__(self, board, depth=100, **kwargs):
        super().__init__(NEGASCOUT, board, depth, **kwargs)


# ---------------------------------------------------------------------
# Export static evaluation to Py for testing.
# ---------------------------------------------------------------------
cpdef eval_static_exchanges(board, Color color, Square square):
    cdef BoardState state = BoardState(board)
    return estimate_static_exchanges(
        state._state, color, square, state._state.piece_type_at(square))


# ---------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------
def get_param_info():
    cdef map[string, Param] info = _get_param_info()
    params = {}
    for elem in info:
        params[elem.first.decode()] = (
            elem.second.val,
            elem.second.min_val,
            elem.second.max_val,
            elem.second.group.decode()
        )
    return params


def set_param(name, value, echo=False):
    _set_param(name.encode(), value, echo)


def get_params():
    cdef map[string, int] cfg = _get_params()
    params = {}
    for elem in cfg:
        params[elem.first.decode()] = elem.second
    return params


def set_hash_size(MB):
    TranspositionTable.set_hash_size(MB)


def get_hash_size():
    return TranspositionTable.get_hash_size()


def get_hash_full():
    return int(TranspositionTable.usage() * 10)


# ---------------------------------------------------------------------
# Ad-hoc move generation performance tests.
# ---------------------------------------------------------------------
def perft(fen, repeat=1):
    cdef vector[Move] moves
    cdef size_t count = 0
    board = BoardState(chess.Board(fen=fen))
    start = time.perf_counter()
    for i in range(0, repeat):
        board._state.generate_pseudo_legal_moves(moves, BB_ALL, BB_ALL)
        count += moves.size()
        # count += board._state.make_pseudo_legal_moves(moves)
    return count, time.perf_counter() - start


def perft2(fen, repeat=1):
    cdef vector[Move] moves
    cdef vector[Move] buffer
    cdef size_t count = 0
    board = BoardState(chess.Board(fen=fen))
    start = time.perf_counter()
    for i in range(0, repeat):
        board._state.generate_moves(moves, buffer)
        count += moves.size()
    return count, time.perf_counter() - start


def perft3(fen, repeat=1):
    cdef TranspositionTable table
    cdef size_t count = 0
    node = NodeContext(chess.Board(fen=fen))
    node._ctxt.set_tt(address(table))
    start = time.perf_counter()

    for i in range(0, repeat):
        node._ctxt._max_depth = max(1, i % 100)
        while node._ctxt.next(False, 0, 0) != NULL:
            count += 1
        node._ctxt.rewind(0, True)

    return count, time.perf_counter() - start


def read_config(fname='sturddle.cfg', echo=False):
    with open(fname) as f:
        ns = {'__builtins__': {}}
        exec(f.read(), ns)

        for name, value in ns.get('params', {}).items():
            set_param(name, value, echo)


# ---------------------------------------------------------------------
# NNUE testing, verify that incremental eval matches full eval results.
# ---------------------------------------------------------------------
def test_incremental_updates(fen):
    cdef TranspositionTable table
    node = NodeContext(chess.Board(fen=fen))
    node._ctxt.set_tt(address(table))
    node._ctxt._max_depth = 50 # avoid LMP

    while True:
        next = node._ctxt.next(False, 0, 0)
        if next == NULL:
            break

        # Full evaluation
        eval_full = next._state.eval_simple()

        # Incremental evaluation from current node state
        eval_incr = node._ctxt._state.eval_incremental(next._move)

        if eval_full != eval_incr:
            move = py_move(next._move).uci()
            raise AssertionError((fen, move, eval_full, eval_incr))


def nnue_init(data_dir, eval_file = NNUE_FILE):
    if not data_dir:
        data_dir = os.path.dirname(__file__)
    data_dir = os.path.realpath(data_dir)
    if NNUE.init(os.path.join(data_dir, '').encode(), eval_file.encode()):
        global NNUE_FILE
        if eval_file != NNUE_FILE:
            NNUE_FILE = eval_file
        return True


def nnue_ok():
    return USE_NNUE


# ---------------------------------------------------------------------
# optional / experimental FEN parsing (needs -DNATIVE_UCI at compile-time)
# ---------------------------------------------------------------------
def board_from_fen(fen: str):
    board = BoardState()
    if parse_fen(fen.encode(), board._state):
        return board


# ---------------------------------------------------------------------
# syzygy tablebases
# ---------------------------------------------------------------------
_tb = chess.syzygy.Tablebase()
_tb_paths = []

def _tb_init():
    for syzygy_path in Context.syzygy_path().decode().split(os.pathsep):
        if not os.path.isabs(syzygy_path):
            syzygy_path = os.path.realpath(
                os.path.join(os.path.dirname(sys.argv[0]), syzygy_path)
            )
        try:
            _tb.add_directory(syzygy_path)
            _tb_paths.append(syzygy_path)
        except:
            pass

    Context.set_syzygy_path(os.pathsep.join(_tb_paths).encode())
    if _tb_paths:
        print(_tb_paths)
    else:
        Context.set_tb_cardinality(0)


cdef bool tb_probe_wdl(const State& state, int* result) except* :
    board = board_from_cxx_state(state)
    try:
        result[0] = _tb.probe_wdl(board)
        return True
    except KeyError as e:
        logging.error(f'tb_probe_wdl: {e}, path={_tb_paths}')

    piece_count = chess.popcount(board.occupied)
    Context.set_tb_cardinality(0 if piece_count <= 3 else piece_count - 1)

    logging.info(f'tb_probe_wdl: cardinality={Context.tb_cardinality()}')
    return False


def set_syzygy_path(path):
    global _tb
    global _tb_paths

    Context.set_syzygy_path(path.encode())
    _tb.close()
    _tb = chess.syzygy.Tablebase()
    _tb_paths = []
    _tb_init()


def syzygy_path():
    return Context.syzygy_path().decode()


# ---------------------------------------------------------------------
# initialize c++ global data structures
# ---------------------------------------------------------------------
Context.init()

NodeContext(chess.Board()) # dummy context initializes static cpython methods
nnue_init(os.path.dirname(__file__))
_tb_init()

__major__   = 1
__minor__   = 20
__build__   = ['NNUE', str(__major__), f'{int(__minor__):02d}', timestamp().decode()]


def version():
    return '.'.join(__build__[not USE_NNUE:])


# ---------------------------------------------------------------------
# in progress: native C++ UCI implementation
# ---------------------------------------------------------------------
def uci(name: str, debug: bool=False):
    cdef string n = name.encode()
    cdef string v = version().encode()
    with nogil:
        run_uci_loop(n.c_str(), v.c_str(), debug)

#!/usr/bin/env python3
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

UCI protocol implementation.
http://wbec-ridderkerk.nl/html/UCIProtocol.html

https://en.wikipedia.org/wiki/Sturddlefish
The sturddlefish is a hybrid of the American paddlefish (Polyodon spathula)
and the Russian sturgeon (Acipenser gueldenstaedtii), accidentally created
by researchers in 2019 and announced in 2020
"""

import argparse
import logging
import time

import chess
import chess.polyglot
from psutil import virtual_memory

import chess_engine as engine
from worker import WorkerThread


NAME = 'Sturddle'

ALGORITHM = { 'mtdf': engine.MTDf_i,
    'negamax': engine.Negamax_i,
    'negascout': engine.Negascout_i,
}


class UCI:
    def __init__(self, args):
        self.args = args
        self.depth = 100
        self.ponder_enabled = False
        self.pondering = False
        self.output_expected = False
        self.extended_time = 0
        self.board = chess.Board()
        self.start_time = time.time()
        self.node_count = 0
        self.i_cb = self.show_thinking if args.show_thinking else None
        self.init_algo()
        self.init_opening_book()

        self.commands = {
            'go': self._go,
            'isready': self._isready,
            'ponderhit': self._ponderhit,
            'position': self._position,
            'quit': self._quit,
            'setoption': self._setoption,
            'stop': self._stop,
            'uci': self._uci,
            'ucinewgame': self._ucinewgame,
        }
        self.worker = WorkerThread() # pondering, 'go infinite' commands


    def init_algo(self):
        name = self.args.algorithm
        self.algorithm = ALGORITHM[name](self.board, self.depth, on_iteration=self.i_cb)
        logging.debug(f'algorithm set to: {self.algorithm}')

    # Possible improvements: support a folder of opening books rather
    # than one single Polyglot file (that's what the lichess bot does),
    # and expose the name of the file (or directory) to UCI setoption.

    def init_opening_book(self):
        self.book = None
        self.use_opening_book = False
        try:
            self.book = chess.polyglot.MemoryMappedReader(self.args.book)
            self.use_opening_book = True
        except FileNotFoundError as e:
            pass
        except:
            logging.exception('opening book')


    def cancel(self):
        self.algorithm.cancel()


    """
    Runs on the background worker thread (supports infinite analysis mode).
    """
    def search_async(self):
        self.algorithm.search(self.board)
        self.pondering = False


    def _go(self, cmd_args):
        self.cancel() # in case there's anything lingering in the background
        self.start_time = time.time()
        self.node_count = 0
        time_remaining = [0, 0]
        movetime = 0
        explicit_movetime = False
        movestogo = 40
        ponder = False
        turn = self.board.turn

        params = iter(cmd_args[1:])
        for a in params:
            if a == 'depth':
                self.depth = int(next(params))
            elif a == 'movetime':
                movetime = int(next(params))
                explicit_movetime = True
            elif a == 'movestogo':
                movestogo = int(next(params))
            elif a == 'wtime':
                time_remaining[chess.WHITE] = int(next(params))
            elif a == 'btime':
                time_remaining[chess.BLACK] = int(next(params))
            elif a == 'ponder':
                ponder = self.ponder_enabled
            elif a == 'infinite':
                movetime = 0

        if not movetime:
            movetime = time_remaining[turn] / max(movestogo, 40)

        # < 1 ms? go async and expect the GUI to tell when to stop
        if movetime < 1:
            movetime = 0

        if self.use_opening_book:
            try:
                entry = self.book.weighted_choice(self.board)
                logging.debug(entry)
                self.output('bestmove ' + entry.move.uci())
                return True
            except IndexError:
                pass
            except:
                logging.exception('opening book')

        logging.debug(f'movetime={movetime:.1f} movestogo={movestogo} fen={self.board.epd()}')

        self.algorithm.depth = self.depth
        self.output_expected = True # The GUI expects a response

        if ponder:
            assert not self.pondering
            self.extended_time = max(1, int(movetime))
            self.algorithm.time_limit_ms = 0
            self.pondering = True
            self.worker.send_message(self._ponder, max_count = 1)
            return True
        else:
            self.extended_time = 0

        # Support 'go infinite' commands (analysis mode):
        # run in background if no time limit, and expect
        # the GUI to send a 'stop' command later.
        if movetime == 0 and self.args.analysis:
            logging.debug('starting infinite search')
            self.pondering = True
            self.worker.send_message(self.search_async, max_count = 1)
            return True

        self.algorithm.time_limit_ms = movetime

        move, _ = self.algorithm.search(
            self.board,
            time_info = None if explicit_movetime else (time_remaining[turn], movestogo)
        )
        # Do not ponder below 1s / move.
        self.output_best(move, request_ponder=(self.ponder_enabled and movetime >= 1000))
        return True


    def _isready(self, *_):
        self.snooze()
        self.output('readyok')
        return True


    def _ponder(self):
        """
        This handler runs on background thread.
        It starts with a zero time limit (infinite search) and expects that:
        either STOP is received, and the _stop handler will set the cancel
        flag and snooze in a loop until the pondering variable becomes False;
        or PONDERHIT is received, which will extend the search by extended_time
        (and set extended_time to zero, to indicate to this function to send out
        the best move when the search finishes).
        Pondering may finish before PONDERHIT is received (if it reaches max depth)
        in which case it resets the extended_time and does not output a best move.
        The _ponderhit handler will send out the best move instead, when PONDERHIT
        is received (thus avoiding "premature bestmove in ponder" errors).
        """
        move, _ = self.algorithm.search(self.board)
        logging.debug(f'pondered: {move}')

        if self.extended_time:
            self.extended_time = 0
        else:
            self.output_best(move, request_ponder=False)

        self.pondering = False


    def _ponderhit(self, *_):
        if self.extended_time:
            logging.debug(f'extend search time by {self.extended_time} ms')
            time_limit = self.extended_time
            self.extended_time = 0
            self.algorithm.extend_time_limit(time_limit)
            return True

        else:
            # zero? assume that's because _ponder already finished;
            # call _stop to send out best move (and also make sure
            # no pondering is left running in the background)

            return self._stop()


    def _position(self, cmd_args):

        # in case the command arrives while pondering...
        self.output_expected = False
        self._stop()

        fen, moves = chess.STARTING_FEN, []
        fen_tok = []
        param = iter(cmd_args[1:])
        for a in param:
            if a == 'fen':
                fen_tok.append(next(param))
            elif a == 'moves':
                moves = [move for move in param]
                break
            elif a != 'startpos':
                fen_tok.append(a)
        if fen_tok:
            fen = ' '.join(fen_tok)
        logging.debug(f'fen="{fen}" moves={moves}')

        self.board.set_fen(fen)
        for move in moves:
            self.board.push(chess.Move.from_uci(move))

        return True


    def _quit(self, *_):
        pass


    def _setoption(self, cmd_args):
        assert len(cmd_args) >= 3
        assert cmd_args[1] == 'name'
        if len(cmd_args) < 4:
            cmd_args.append('value')
        if len(cmd_args) < 5:
            assert cmd_args[3] == 'value'
            cmd_args.append('')

        name, value = cmd_args[2], cmd_args[4]
        if name == 'OwnBook':
            self.use_opening_book = value == 'true'
        elif name == 'Ponder':
            self.ponder_enabled = value == 'true'
        elif name == 'Algorithm':
            self.args.algorithm = value
            self.init_algo()
        elif name == 'SyzygyPath':
            engine.set_syzygy_path(value)
        else:
            if value in ['true', 'false']:
                engine.set_param(name, value == 'true')
            else:
                engine.set_param(name, int(value))
            logging.debug(f'{name}={engine.get_param_info()[name]}')
        return True


    def snooze(self):
        while self.pondering:
            snooze = max(0.001, self.extended_time / 1000000)
            logging.debug(f'snoozing {snooze:.3f} seconds')
            time.sleep(snooze)

            # cancel again, to deal with the race condition of receiving
            # STOP before the background pondering had the chance to start
            self.cancel()


    def _stop(self, *_):
        self.cancel()
        self.snooze()

        if self.output_expected:
            move = self.algorithm.best_move
            self.output_best(move, request_ponder = False)

        return True


    def _uci(self, *_):
        self.output(f'id name {NAME}-{engine.version()}-{args.algorithm}')
        self.output('id author cristi.vlasceanu@gmail.com')

        self.output(f'option name Algorithm type combo default {self.args.algorithm} ' + \
            ' '.join([f'var {k}' for k in ALGORITHM.keys()]))
        if self.book:
            self.output('option name OwnBook type check default true')
        self.output('option name Ponder type check')
        self.output(f'option name SyzygyPath type string default {engine.syzygy_path()}')

        if self.args.tweak:
            whitelist = engine.get_param_info().keys()
        else:
            whitelist = ['Hash', 'SEE', 'Threads']

        for name, (val, val_min, val_max, _) in engine.get_param_info().items():
            if name not in whitelist or name.startswith('DEBUG_'):
                continue
            if val_min == 0 and val_max == 1:
                val = 'true' if val else 'false'
                self.output(f'option name {name} type check default {val}')
            else:
                self.output(f'option name {name} type spin default {val} min {val_min} max {val_max}')

        self.output('uciok')
        return True


    def _ucinewgame(self, *_):
        engine.clear_hashtable()
        self.init_algo()
        return self._position(['position', chess.STARTING_FEN])


    def output(self, message):
        print(message, flush = True)
        logging.debug(f'<<< {message}')


    def output_best(self, move, request_ponder=True):
        if self.output_expected:
            try:
                if move is None:
                    self.output('resign')
                else:
                    uci = move.uci()
                    if self.ponder_enabled and request_ponder:
                        pv = self.algorithm.get_pv()
                        if len(pv) > 1 and pv[0] == uci:
                            self.output(f'bestmove {uci} ponder {pv[1]}')
                            return
                    self.output(f'bestmove {uci}')
            finally:
                self.output_expected = False


    def run(self):
        while True:
            cmd = input()
            if not cmd:
                logging.error('No command.')
                break
            logging.debug(f'>>> {cmd}')

            cmd_args = cmd.split()
            cmd_handler = self.commands.get(cmd_args[0], None)

            if not cmd_handler:
                logging.warning(f'not understood: {cmd_args}')
                continue

            # dispatch cmd and arguments to command handler
            try:
                if not cmd_handler(cmd_args):
                    logging.debug(f'{cmd_args}: good bye.')
                    break
            except:
                logging.exception(f'exception in command handler, cmd="{cmd}", args: {cmd_args}')

                # fail hard and fast when in tuning mode
                if args.tweak:
                    raise


    def show_thinking(self, algorithm, node, score, node_count, knps, ms):
        if node.task_id:
            return
        try:
            score_info = f'score cp {score}'

            # principal variation
            pv = self.validate_pv(node.get_pv())
            seldepth = max(len(pv), algorithm.current_depth)

            # mate in moves, NOT plies
            if score > engine.SCORE_MATE_HIGH:
                mate = (max(engine.SCORE_CHECKMATE - score, len(pv)) + 1) // 2
                logging.debug(f'score={score}, pv={len(pv)}, mate={mate}')
                score_info += f' mate {mate}'
            elif score < engine.SCORE_MATE_LOW:
                mate = -(max(engine.SCORE_CHECKMATE + score, len(pv)) + 1) // 2
                logging.debug(f'score={score}, pv={len(pv)}, mate={mate}')
                score_info += f' mate {mate}'

            if pv := ' '.join(pv):
                pv = 'pv ' + pv

            depth = f'depth {algorithm.current_depth} seldepth {seldepth}'
            nodes = f'nodes {node_count} nps {int(knps * 1000)}'

            hashfull = f'hashfull {engine.get_hash_full()}'
            self.output(f'info {depth} {score_info} time {ms} {hashfull} {nodes} {pv}')

        except Exception as e:
            logging.exception(e)


    def validate_pv(self, pv):
        if self.args.debug:
            # valid_pv = []
            board = chess.Board()
            self.algorithm.context.state.copy_to_board(board)
            start = board.copy()
            try:
                for move in pv:
                    board.push(chess.Move.from_uci(move))
                    assert board.is_valid()
                    # valid_pv.append(move)
            except:
                logging.exception(f'{move}: pv={pv}, pos={start.epd()}')
                # pv = valid_pv
        return pv


def main(args):
    try:
        UCI(args).run()
    except:
        logging.exception('exiting main()')


def configure_logging(args):
    log = logging.getLogger()

    for h in log.handlers[:]:
        log.removeHandler(h)

    log_format = '%(asctime)s %(levelname)-8s %(process)d %(message)s'
    log_level = logging.DEBUG if args.debug else logging.INFO

    logging.basicConfig(level = log_level, filename = args.logfile, format = log_format)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-a', '--algorithm', choices=ALGORITHM.keys(), default='mtdf')
    parser.add_argument('--analysis', action='store_true')
    parser.add_argument('-b', '--book', default='book.bin')
    parser.add_argument('-c', '--config')
    parser.add_argument('-d', '--debug', action='store_true') # enable verbose logging
    parser.add_argument('-l', '--logfile', default='uci.log')
    parser.add_argument('--tweak', action='store_true')
    parser.add_argument('--show-thinking', dest='show_thinking', action='store_true')
    parser.add_argument('--no-show-thinking', dest='show_thinking', action='store_false')

    parser.set_defaults(show_thinking=True)

    args = parser.parse_args()
    configure_logging(args)

    print (f'{NAME}-{engine.version()} UCI')

    if args.config:
        engine.read_config(args.config)
        logging.info(f'Read configuration file {args.config}')

    main(args)

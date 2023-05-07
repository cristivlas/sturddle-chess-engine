#! /usr/bin/env python3
'''
Alternative engine bootloader that uses the native UCI implementation.
'''
# import everything for the benefit of pyinstaller
import argparse
import importlib
import logging
import math
import os
import sysconfig
import time

import chess
import chess.pgn
import chess.polyglot
import chess.syzygy
import psutil

platform = sysconfig.get_platform()
if any(('arm' in platform, 'aarch64' in platform)):
    flavors = { 'chess_engine': lambda *_: True }
else:
    import cpufeature
    '''
    Import the chess engine module flavor that best matches the CPU capabilities.
    '''
    def _is_avx512_supported():
        for f in cpufeature.extension.CPUFeature:
            if (f.startswith('AVX512') and cpufeature.extension.CPUFeature[f]):
                return True
        return False

    def _is_avx2_supported():
        return cpufeature.extension.CPUFeature['AVX2']

    flavors = {
        'chess_engine_avx512': _is_avx512_supported,
        'chess_engine_avx2': _is_avx2_supported,
        'chess_engine': lambda *_: True,
    }

def load_engine():
    for eng in flavors:
        if not flavors[eng]():
            continue
        try:
            engine = importlib.import_module(eng)
            globals().update({k:v for k, v in engine.__dict__.items() if not k.startswith('_')})
            return engine
        except Exception as e:
            logging.warning(e)

def _configure_logging(args):
    log = logging.getLogger()
    for h in log.handlers[:]:
        log.removeHandler(h)
    format = '%(asctime)s %(levelname)-8s %(process)d %(message)s'
    filename = f'{args.logfile}.{os.getpid()}' if args.separate_logs else args.logfile
    logging.basicConfig(level=logging.INFO, filename=filename, format=format)

'''
Workaround for --onefile executable built with PyInstaller:
hide the console if not running from a CMD prompt or Windows Terminal
'''
def _hide_console():
    # Running under Windows, but not from under CMD.EXE or Windows Terminal (PowerShell)?
    if sysconfig.get_platform().startswith('win') and all(
        (v not in os.environ) for v in ['PROMPT', 'WT_SESSION']
        ):
        # Grandparent is None if running under Python interpreter in CMD.
        p = psutil.Process().parent().parent()
        # Make an exception and show the window if started from explorer.exe.
        if p and p.name().lower() != 'explorer.exe':
            import ctypes
            ctypes.windll.user32.ShowWindow(ctypes.windll.kernel32.GetConsoleWindow(), 0)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Sturddle Chess Engine bootloader')
    parser.add_argument('-l', '--logfile', default='sturddle.log')
    parser.add_argument('-s', '--separate-logs', action='store_true')
    parser.add_argument('-v', '--verbose', action='store_true', help='enable verbose logging')
    args = parser.parse_args()
    _configure_logging(args)
    _hide_console()

    engine = load_engine()
    assert engine, 'Failed to load engine.'
    try:
        engine.uci('Sturddle UCI', debug=args.verbose)
    except KeyboardInterrupt:
        pass
    except:
        logging.exception('UCI')

#! /usr/bin/env python3
'''
Alternative engine bootloader that uses the native UCI implementation.
'''
import argparse
import importlib
import logging

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

for eng in flavors:
    if not flavors[eng]():
        continue
    try:
        engine = importlib.import_module(eng)
        globals().update({k:v for k, v in engine.__dict__.items() if not k.startswith('_')})
        break
    except:
        pass

def _configure_logging(args):
    log = logging.getLogger()
    for h in log.handlers[:]:
        log.removeHandler(h)
    format = '%(asctime)s %(levelname)-8s %(process)d %(message)s'
    logging.basicConfig(level=logging.INFO, filename=args.logfile, format=format)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Sturddle Chess Engine bootloader')
    parser.add_argument('-l', '--logfile', default='sturddle.log')
    args = parser.parse_args()
    _configure_logging(args)
    try:
        engine.uci('Sturddle')
    except KeyboardInterrupt:
        pass

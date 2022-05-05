#! /usr/bin/env python3
#
# Generate config for chess-tuning-tools
# https://chess-tuning-tools.readthedocs.io/en/latest/
#
import argparse
import json
import os
import sys

import psutil

def root_path():
    return os.path.abspath(os.path.join(os.path.split(sys.argv[0])[0], '..'))


def make_path(*args):
    return os.path.abspath(os.path.join(root_path(), *args))


sys.path.append(root_path())
from chess_engine import *


fixed_params = {
}


def engine(tweak=False, **kwargs):
    command = make_path('sturddle.py')
    if tweak:
        command += ' --tweak'

    command += ' --no-show-thinking'

    for p in fixed_params:
        kwargs[p] = fixed_params[p]

    return {
        'command': f'{sys.executable} {command}',
        'fixed_parameters': kwargs,
    }


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-a', '--asymmetric', action='store_true')
    parser.add_argument('-b', '--book', default='8moves_v3')
    parser.add_argument('-c', '--concurrency', type=int, default=os.cpu_count() // 2)
    parser.add_argument('--hash', type=int)
    parser.add_argument('-p', '--plot-every', type=int, default=20)
    parser.add_argument('-r', '--rounds', type=int)
    parser.add_argument('-s', '--smp-cores', type=int, default=1)
    parser.add_argument('-t', '--time-control', default='1+0.1')

    params = {}
    groups = set()

    for name, (val, lo, hi, grp) in  get_param_info().items():
        if grp == 'Settings':
            continue
        groups.add(grp)
        params[name] = lo, hi, grp

    parser.add_argument('tune', choices=params.keys(), nargs='+')
    parser.add_argument('-z', '--zero-groups', choices=groups, nargs='*')

    args = parser.parse_args()

    if args.hash:
        fixed_params['Hash'] = args.hash
    else:
        fixed_params['Hash'] = int(psutil.virtual_memory().available / (1024 * 1024 * 2.5 * args.concurrency))

    fixed_params['Threads'] = args.smp_cores

    if args.zero_groups:
        for p in params:
            g = params[p][2]
            if (g in args.zero_groups) and (p not in args.tune):
                fixed_params[p] = 0 if 0 in params[p] else params[p][0]

    tune_params = {}
    for p in args.tune:
        lo, hi, _ = params[p]
        tune_params[p] = f'({lo}, {hi})'

    config = {
        'engines': [
            engine(tweak=True, OwnBook=False, Algorithm='mtdf'),
            engine(tweak=True, OwnBook=False, Algorithm='negascout' if args.asymmetric else 'mtdf'),
        ],
        'parameter_ranges': tune_params,
        'acq_function': 'vr',
        'engine1_tc': args.time_control,
        'engine2_tc': args.time_control,
        'rounds': args.rounds if args.rounds else len(args.tune) * 15,
        'plot_every': args.plot_every,
        'opening_file': make_path('tuneup', 'books', args.book + '.pgn'),
        'concurrency': args.concurrency
    }

    print(json.dumps(config, sort_keys=False, indent=4))

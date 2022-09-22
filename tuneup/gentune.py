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

desc='Generate JSON config for tuneup (see https://chess-tuning-tools.readthedocs.io/en/latest/)'


def root_path():
    return os.path.abspath(os.path.join(os.path.split(sys.argv[0])[0], '..'))


def make_path(*args):
    return os.path.abspath(os.path.join(root_path(), *args))


sys.path.append(root_path())
from chess_engine import *


fixed_params = {
}


def engine(command, **kwargs):

    for p in fixed_params:
        kwargs[p] = fixed_params[p]

    return {
        'command': command,
        'fixed_parameters': kwargs,
    }


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('-a', '--asymmetric', action='store_true', help='test mtdf vs. negascout')
    parser.add_argument('-b', '--book', default='8moves_v3', help='opening book file')
    parser.add_argument('-c', '--concurrency', type=int, default=os.cpu_count() // 2)
    parser.add_argument('--hash', type=int, help='hash table size')
    parser.add_argument('-o', '--output')
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

    tunable = tuple(['all'] + list(params.keys()))
    parser.add_argument('tune', choices=tunable, nargs='+')
    parser.add_argument('-z', '--zero-groups', choices=groups, nargs='*')

    args = parser.parse_args()

    # strip 'all'
    _, *tunable = tunable

    # substitute 'all'
    args.tune = set(p if p != 'all' else q for q in tunable for p in args.tune)

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

    command = make_path('sturddle.py')

    config = {
        'engines': [
            engine(command, OwnBook=False, Algorithm='mtdf'),
            engine(command, OwnBook=False, Algorithm='negascout' if args.asymmetric else 'mtdf'),
        ],
        'parameter_ranges': tune_params,
        'acq_function': 'pvrs',
        'adjudicate_resign': True,
        'engine1_tc': args.time_control,
        'engine2_tc': args.time_control,
        'rounds': args.rounds if args.rounds else len(args.tune) * 15,
        'plot_every': args.plot_every,
        'opening_file': make_path('tuneup', 'books', args.book + '.pgn'),
        'concurrency': args.concurrency
    }

    config = json.dumps(config, sort_keys=False, indent=4)
    if not args.output:
        print(config)
    else:
        with open(args.output, 'w') as file_out:
            file_out.write(config)


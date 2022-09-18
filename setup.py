from setuptools import Extension, setup
from Cython.Build import cythonize
from os import environ
import sysconfig

sourcefiles = [
    '__init__.pyx',
    'captures.cpp',
    'chess.cpp',
    'context.cpp',
    'search.cpp',
    'nnue-probe/src/misc.cpp',
    'nnue-probe/src/nnue.cpp',
]

platform = sysconfig.get_platform()
"""
Compiler args.
"""
inc_dirs = [
    '-I./libpopcnt',
    '-I./magic-bits/include',
    '-I./nnue-probe/src',
]

link = []

# Release build
args = ['-DNO_ASSERT']

# Assert-enabled build
if environ.get('BUILD_ASSERT', None):
    args = ['-DTUNING_ENABLED=true']

# Debug build
if environ.get('BUILD_DEBUG', None):
    if platform.startswith('win'):
         args = [ '/Od', '/Zi' ]
         link = ['/DEBUG']
    else:
         args = [ '-O0', '-D_DEBUG', '-DTUNING_ENABLED' ]


if platform.startswith('win'):
    args.append('/std:c++17')
else:
    if '-O0' not in args:
        args.append('-O3')
    args.append('-std=c++17')
    args.append('-Wall')
    args.append('-Wextra')
    args.append('-Wno-unused-label')
    args.append('-Wno-unknown-pragmas')
    args.append('-Wno-unused-parameter')
    args.append('-Wno-unused-variable')
    args.append('-DCYTHON_WITHOUT_ASSERTIONS')
    #args.append('-fprofile-sample-use=code.prof')

    cc = 'clang' if platform.startswith('macos') else environ.get('CC', None)
    if cc and cc.startswith('clang'):
        args.append('-Wno-deprecated-declarations')

    ### nnue-probe ###
    args += [
        '-DUSE_AVX2',
        '-mavx2',
        '-DUSE_SSE41',
        '-msse4.1',
        '-DUSE_SSE3',
        '-msse3',
        '-DUSE_SSE2',
        '-msse2',
        '-DUSE_SSE',
        '-msse'
    ]
"""
end of compiler args.
"""

extensions = [
    Extension(
        'chess_engine',
        sources=sourcefiles,
        extra_compile_args=args + inc_dirs,
        extra_link_args=link
    )
]

setup(ext_modules=cythonize(extensions))


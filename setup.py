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
    args += [
        '-std=c++17',
        '-Wall',
        '-Wextra',
        '-Wno-unused-label',
        '-Wno-unknown-pragmas',
        '-Wno-unused-parameter',
        '-Wno-unused-variable',
        '-DCYTHON_WITHOUT_ASSERTIONS',
        #'-fprofile-sample-use=code.prof',
    ]
    # Silence off Py_DEPRECATED warnings for clang
    cc = 'clang' if platform.startswith('macos') else environ.get('CC', None)
    if cc and cc.startswith('clang'):
         args.append('-Wno-deprecated-declarations')
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


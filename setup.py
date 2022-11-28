from setuptools import Extension, setup
from Cython.Build import cythonize
from os import environ
from datetime import datetime
import sysconfig

build_stamp = datetime.now().strftime('%m%d%y.%H%M')


sourcefiles = [
    '__init__.pyx',
    'captures.cpp',
    'chess.cpp',
    'context.cpp',
    'search.cpp',
    'nnue-probe/src/misc.cpp',
    'nnue-probe/src/nnue.cpp',
]


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

platform = sysconfig.get_platform()

# Experimental
FEN_PARSE = environ.get('FEN_PARSE', '').lower() in ['1', 'true', 'yes']

# Debug build
if environ.get('BUILD_DEBUG', None):
    if platform.startswith('win'):
        args = [ '/Od', '/Zi' ]
        link = ['/DEBUG']
    else:
        args = [ '-O0', '-D_DEBUG', '-DTUNING_ENABLED' ]


args.append('-DBUILD_STAMP=' + build_stamp)


if platform.startswith('win'):
    args += environ.get("CXXFLAGS", '').split()
    args += [
        '/std:c++17',
        '/DWITH_NNUE',
        '/DCALLBACK_PERIOD=8192',
    ]
    if environ.get('CL_EXE', '')=='clang-cl.exe':
        args += [
            '-Ofast',
            '-Wno-unused-command-line-argument',
            '-Wno-unused-variable',
        ]
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
        '-DCALLBACK_PERIOD=8192',
        '-fno-stack-protector',
        '-fvisibility=hidden',
        '-DPyMODINIT_FUNC=__attribute__((visibility("default"))) extern "C" PyObject*',
        '-DWITH_NNUE',
    ]
    if FEN_PARSE:
        args += [
            '-std=c++20',
            '-fexperimental-library',
            '-DFEN_PARSE=true',
        ]
        link += ['-L/usr/local/opt/llvm/lib/c++']

    # Silence off Py_DEPRECATED warnings for clang;
    # clang is the default compiler on macosx.
    cc = 'clang' if platform.startswith('macos') else environ.get('CC', None)
    if cc and cc.startswith('clang'):
         args.append('-Wno-deprecated-declarations')
"""
end of compiler args.
"""

extensions = [
    Extension(
        name=environ.get('TARGET', 'chess_engine'),
        sources=sourcefiles,
        extra_compile_args=args + inc_dirs,
        extra_link_args=link
    ),
    Extension(
        name='uci',
        sources=['uci.pyx']
    )
]

setup(ext_modules=cythonize(extensions))

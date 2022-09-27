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

# Debug build
if environ.get('BUILD_DEBUG', None):
    if platform.startswith('win'):
         args = [ '/Od', '/Zi' ]
         link = ['/DEBUG']
    else:
         args = [ '-O0', '-D_DEBUG', '-DTUNING_ENABLED' ]


args.append('-DBUILD_STAMP=' + build_stamp)


if environ.get('BUILD_WITH_NNUE', None):
    args.append('-DWITH_NNUE')


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
        '-fvisibility=hidden',
        '-DPyMODINIT_FUNC=__attribute__((visibility("default"))) extern "C" PyObject*',
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


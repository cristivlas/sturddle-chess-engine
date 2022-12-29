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
    'uci_native.cpp',
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
NATIVE_UCI = environ.get('NATIVE_UCI', '').lower() in ['1', 'true', 'yes']

# Debug build
if environ.get('BUILD_DEBUG', None):
    if platform.startswith('win'):
        args = [ '/Od', '/Zi' ]
        link = ['/DEBUG']
    else:
        args = [ '-O0', '-D_DEBUG', '-DTUNING_ENABLED' ]


args.append('-DBUILD_STAMP=' + build_stamp)
args += environ.get("CXXFLAGS", '').split()

if platform.startswith('win'):
    # Windows build
    args += [
        '/std:c++20',
        '/DWITH_NNUE',
        '/DCALLBACK_PERIOD=8192',
    ]
    if NATIVE_UCI:
        args.append('/DNATIVE_UCI=true')

    if environ.get('CL_EXE', '')=='clang-cl.exe':
        args += [
            '-Ofast',
            '-Wno-unused-command-line-argument',
            '-Wno-unused-variable',
        ]
else:
    # Linux and Mac
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
        '-DWITH_NNUE',
    ]
    if NATIVE_UCI:
        args += [
            '-std=c++20',
            '-stdlib=libc++',
            '-fexperimental-library',
            '-DNATIVE_UCI=true',
        ]
        link += [
            '-fuse-ld=lld',
            '-L/usr/lib/llvm-15/lib/',
            '-L/usr/local/opt/llvm/lib/c++',
            '-lc++',
            '-lc++experimental',
        ]
    # Silence off Py_DEPRECATED warnings for clang;
    # clang is the default compiler on macosx.
    cc = 'clang' if platform.startswith('macos') else environ.get('CC', None)
    if cc and cc.startswith('clang'):
        args += [
            '-Wno-deprecated-declarations',
            '-fvisibility=hidden',
            '-DPyMODINIT_FUNC=__attribute__((visibility("default"))) extern "C" PyObject*',
        ]
"""
end of compiler args.
"""

extensions = [
    Extension(
        name=environ.get('TARGET', 'chess_engine'),
        sources=sourcefiles,
        extra_compile_args=args + inc_dirs,
        extra_link_args=link
    )]
if not NATIVE_UCI:
    extensions.append(Extension(name='uci', sources=['uci.pyx']))


setup(ext_modules=cythonize(extensions))

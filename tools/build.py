'''
Build all-in-one executable.
'''
import argparse
import glob
import os
import secrets
import shutil
import string
import sys

BOOK = 'book.bin'
NNUE = 'nn-62ef826d1a6d.nnue'

def delete_file_or_dir(path):
    paths = glob.glob(path)
    for p in paths:
        if os.path.exists(p):
            if os.path.isfile(p):
                os.remove(p)
            else:
                shutil.rmtree(p)

def delete(list):
    for path in list:
        delete_file_or_dir(path)

def is_windows():
    return os.name == 'nt' or sys.platform in ['win32', 'cygwin']

def run_cmd(command):
    print(command)
    return os.system(command)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='build all-in-one executable')
    parser.add_argument('-v', '--venv')
    parser.add_argument('--native-uci', dest='native_uci', action='store_true', default=True)
    parser.add_argument('--no-native-uci', dest='native_uci', action='store_false')

    args = parser.parse_args()

    if args.native_uci:
        os.environ['NATIVE_UCI'] = '1'

    mods = '*.pyd' if is_windows() else '*.so'

    # cleanup
    delete(['*.spec', 'dist', 'build', mods])

    exe = sys.executable # the Python interpreter

    for arch in ['AVX512', 'AVX2', '']:
        delete(['uci.cpp', '__init__.cpp']) # force re-cythonize
        print('*********************************************************')
        print(f'Building {arch if arch else "generic"} module')
        print('*********************************************************')

        arch_spec = ''
        if is_windows():
            arch_flag = f'/arch:{arch}'
        # otherwise assume Clang compiler
        elif arch == 'AVX2':
            arch_flag = '-march=core-avx2'
        elif arch == 'AVX512':
            arch_flag = '-march=skylake-avx512'

        os.environ['CXXFLAGS'] = arch_spec
        arch = arch.lower()
        os.environ['TARGET'] = f'chess_engine_{arch}' if arch else 'chess_engine'

        if run_cmd(f'{exe} setup.py clean --all') or run_cmd(f'{exe} setup.py build_ext --inplace'):
            print('Build failed.')
            sys.exit(-1)

    # Generate key
    KEY = ''.join(secrets.choice(string.ascii_uppercase + string.ascii_lowercase) for i in range(16))

    if args.venv:
        scripts_dir = 'Scripts' if is_windows() else 'bin'
        installer = os.path.join(args.venv, scripts_dir, 'pyinstaller')
    else:
        installer = 'pyinstaller'

    # PyInstaller arguments:
    script = 'main.py' if args.native_uci else 'sturddle.py'

    libs = [ f'--add-binary={mods}{os.path.pathsep}.' ]

    if args.native_uci:
        for libcxx in [
            '/usr/lib/llvm-15/lib/libc++.1.so',
            '/usr/lib/llvm-15/lib/libc++abi.1.so',
            '/usr/local/opt/llvm/lib/c++/libc++.1.dylib',
            '/usr/local/opt/llvm/lib/c++/libc++abi.1.dylib'
        ]:
            if os.path.exists(libcxx):
                libs.append(f'--add-binary={libcxx}{os.path.pathsep}.')

    data = f'--add-data={NNUE}{os.path.pathsep}. --add-data={BOOK}{os.path.pathsep}.'

    # run PyInstaller
    if run_cmd(f'{installer} {script} -p . --onefile {" ".join(libs)} {data} --key="{KEY}"'):
        print('pyinstaller failed')
        sys.exit(-2)

    # import the engine we just built, to determine its version
    sys.path.append('.')
    import chess_engine

    MAIN = os.path.join('dist', 'main' if args.native_uci else 'sturddle')
    NAME = os.path.join('dist', f'sturddle-{".".join(chess_engine.__build__[1:3])}')
    if is_windows():
        MAIN += '.exe'
        NAME += '.exe'

    while True:
        try:
            print(f'rename {MAIN} as {NAME}')
            os.rename(MAIN, NAME)
            break
        except Exception as e:
            print(e)
            os.unlink(NAME)

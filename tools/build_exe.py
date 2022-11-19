'''
Build all-in-one Windows executable.
'''
import os
import sys

BOOK = 'book.bin'
NNUE = 'nn-62ef826d1a6d.nnue'

if __name__ == '__main__':
    #os.environ['CL_EXE'] = 'clang-cl.exe'

    os.system('del *.pyd')
    os.system('del sturddle.spec')

    for arch in ['AVX512', 'AVX2', '']:
        os.system('del __init__.cpp')
        print('*********************************************************')
        print(f'Building {arch}')
        print('*********************************************************')
        os.environ['CXXFLAGS'] = f'/arch:{arch}' if arch else ''
        arch = arch.lower()
        os.environ['TARGET'] = f'chess_engine_{arch}' if arch else 'chess_engine'
        os.system(f'{sys.executable} setup.py clean --all')
        os.system(f'{sys.executable} setup.py build_ext --inplace')

    sys.path.append('.')
    import chess_engine
    NAME = f'sturddle-{".".join(chess_engine.__build__[1:3])}.exe'

    # Use virtual environment
    os.system(f'py3.11\\Scripts\\pyinstaller.exe sturddle.py -p . --onefile --add-binary=chess_engine*.pyd;. --hidden-import chess.pgn --hidden-import chess.syzygy --add-data={NNUE};. --add-data={BOOK};.')

    while True:
        try:
            os.rename('dist/sturddle.exe', f'dist/{NAME}')
            break
        except:
            os.unlink(f'dist/{NAME}')

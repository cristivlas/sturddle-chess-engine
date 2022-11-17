import importlib

flavors = [
    'chess_engine_avx512',
    'chess_engine_avx2',
    'chess_engine'
]

for m in flavors:
    try:
        engine = importlib.import_module(m)
        globals().update(engine.__dict__)
        break
    except:
        pass

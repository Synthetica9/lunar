./scripts/c_chess_cli.py \
  -engine cmd=stockfish tc=2+0.2 name=goodfish \
  -engine cmd=stockfish tc=1+0.1 name=badfish \
  -draw number=40 count=8 score=10 \
  -resign count=3 score=500 \
  -repeat \
  -sample decay=.02 resolve=y file=test.csv \
  -games 400 \
  -openings file=./test_data/blitz_openings.fen

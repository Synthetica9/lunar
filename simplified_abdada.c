
/*
        Pseudocode for Simplified ABDADA
        Copyright 2017 Tom Kerrigan
        There are notes at the end of this file.
*/

#define DEFER_DEPTH 3
#define CS_SIZE 32768 // CS = "currently_searching"
#define CS_WAYS 4

volatile int currently_searching[CS_SIZE][CS_WAYS];

// return true if a move is being searched

bool defer_move(int move_hash, int depth) {
  int n, i;
  if (depth < DEFER_DEPTH) // note 1
    return false;
  n = move_hash & (CS_SIZE - 1);
  for (i = 0; i < CS_WAYS; i++) // note 2
  {
    if (currently_searching[n][i] == move_hash)
      return true;
  }

  return false;
}

void starting_search(int move_hash, int depth) {
  int n, i;
  if (depth < DEFER_DEPTH)
    return;

  n = move_hash & (CS_SIZE - 1);
  for (i = 0; i < CS_WAYS; i++)

  {
    if (currently_searching[n][i] == 0) {
      currently_searching[n][i] = move_hash;
      return;
    }

    if (currently_searching[n][i] == move_hash) // note 3.1
      return;
  }

  currently_searching[n][0] = move_hash;
}

void finished_search(int move_hash, int depth) {
  int n, i;
  if (depth < DEFER_DEPTH)
    return;

  n = move_hash & (CS_SIZE - 1);
  for (i = 0; i < CS_WAYS; i++) {
    if (currently_searching[n][i] == move_hash) // note 3.2
      currently_searching[n][i] = 0;
  }
}

int search(int alpha, int beta, int depth) {
  ... generate_moves();

  for (i = 0; i < moves; i++) {
    if (i == 0) // first move
    {
      make_move(move[i]);
      x = -search(-beta, -alpha, depth - 1); // note 5.1
      undo_move();
    } else {
      int move_hash;

      // PH: this is just the zobrist hash but shite.
      move_hash = current_position_hash;
      move_hash ^= (move[i] * 1664525) + 1013904223; // note 4

      if (defer_move(move_hash, depth)) {
        // PH: deferred_moves.push(ply)
        deferred_move[deferred_moves++] = move[i];
        continue;
      }

      make_move(move[i]);
      starting_search(move_hash, depth) x =
          -search(-alpha - 1, -alpha, depth - 1); // null-window search
      finished_search(move_hash, depth);
      if (x > alpha && x < beta)
        x = -search(-beta, -alpha, depth - 1); // note 5.2
      undo_move();
    }

    if (x > alpha)
      // PH: what/do?
      ...
  }

  for (i = 0; i < deferred_moves; i++) {
    make_move(deferred_move[i]);
    x = -search(-alpha - 1, -alpha, depth - 1); // note 5.3
    if (x > alpha && x < beta)
      x = -search(-beta, -alpha, depth - 1);
    undo_move();
    if (x > alpha)
      ...
  }
  ...
}

/*


Note 1:

DEFER_DEPTH reduces memory traffic by preventing access to currently_searching
at shallow depths.



Note 2:

currently_searching is an n-way hash table to address collisions.



Note 3:

It isn't necessary to synchronize access to currently_searching.
Nothing bad happens if defer_move occasionally returns the wrong value.
That being said, the code has to handle race conditions:

        Note 3.1:
                Two threads can call starting_search() with the same move.

        Note 3.2:
                Two threads may have written the same move to different places.
                Don't stop after zeroing out one move.

Note 4:

This code assumes that move[i] is a binary representation of the move, e.g., one
byte for the source
square, one byte for the destination square, etc.

To evenly distribute the move in the hash key space, imagine that it's the seed
for a linear
congruential pseudo-random number generator:

https://en.wikipedia.org/wiki/Linear_congruential_generator

Note 5:

There are calls to search() that don't have corresponding calls to
starting_search() and

finished_search().

This is because the practical effect of calling starting_search() is to make
other threads search a

move later. In some cases, this is unnecessary or undesirable:

        Note 5.1:

                Threads always search the first move first.

                Calling starting_search() on the first move would have no
effect.

        Note 5.2:
                If a move fails a null-window search, it's probably going to
fail high or change alpha.
                Other threads should help search the move, not search it later.

        Note 5.3:
                Deferred moves have probably already been searched.
                There's no reason to make other threads search a move later if
it's already been searched.
                If a deferred move is still being searched, it may be failing
high, and other threads should
                help search it.


VERSION HISTORY
Version 1.0, 8/24/17
        - First version


Version 1.1, 8/28/17
        - Switched currently_searching to an n-way hash table to address
collisions.

*/

A Haskell beginner's attempt at a Brainf**k interpreter.  Undertaken as a learning exercise.  

Later versions incorporate ideas from other haskell versions, and Tekmo's notes on reddit: http://www.reddit.com/r/haskell/comments/woqhb/fhck_10_a_brainfuck_interpreter_written_in_haskell

See various branches for slightly more advanced implementations.

Notes:

On rollover, and memory cell size:
Some programs have been written to rely on rollover.  For example, the 99bottles.bf used in testing this interpreter requires that rollover happen when decrementing memory values.  Initially, I had the functions simulate rollover manually.  I've since switched to a data type (Data.Int.Int8) which performs rollover itself.


Representing the memory:
Initially I used an array from Data.Array for the memory.  However, it turns out that the entire array is copied when you modify this array.  
Then I switched to a Map between address and "byte", which gave reasonable performance.  
Then I switched to a Zipper, experimentally, after reading other haskell versions (and Tekmo's review on reddit).  A zipper is particularly suitable in this problem, as there is no random access in brainfk; the memory "pointer" is incremented and decremented only ever by one location per instruction. see commit a0ccda8057287e7cfa882addf36a56bdc4c86951


Monadic and non-monadic approaches: 
Branches:
Branch "master": Currently this contains a simple, non-monadic implementation.  The state (World) is explicitly passed around.
Branch "state_and_io": 
  1) Starts by using a simple, hand-rolled state monad, then 
  2) progresses to use the official one, then 
  3) switches to use state monad transformer with IO as an inner monad.
This evolution allowed me to see what code needed to be changed as more powerful representations were brought in.  I actually find the simple non-monadic version suitable, at least while the interpreter has such limited functionality.  Hence, that version remains in master.


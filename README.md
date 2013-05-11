A Haskell Beginner's attempt at a Brainfk interpreter.  Just a learning exercise.

Notes:

On rollover, and memory cell size:
Some programs have been written to rely on rollover.  For example, the 99bottles.bf used in testing this interpreter requires that rollover happen when decrementing memory values.  Initially, I had the functions simulate rollover manually.  I've since switched to a data type (Data.Int.Int8) which performs rollover itself.

Learning Notes:

Performance:

Representing the memory:
Initially I used an array from Data.Array for the memory.  However, it turns out that the entire array is copied when you modify this array.  
Then I switched to a Map between address and "byte", which gave reasonable performance.  
Then I switched to a Zipper, experimentally, after reading other haskell versions (and Tekmo's review on reddit).  A zipper is particularly suitable in this problem, as there is no random access in brainfk; the memory "pointer" is incremented and decremented only ever by one location per instruction. see commit a0ccda8057287e7cfa882addf36a56bdc4c86951



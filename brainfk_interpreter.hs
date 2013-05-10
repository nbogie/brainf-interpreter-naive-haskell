-- Incomplete, as it doesn't do input
module Main where
import Data.Array
import Data.Char (chr, ord)
import Data.Int (Int8)
import System.Environment (getArgs)
import Test.HUnit

type Byte = Int8
--------------------------------------------------------------------------------
-- zipper
--------------------------------------------------------------------------------

type Zipper a = ([a], a, [a])
zFwd,zBack :: Zipper a -> Zipper a
zFwd (xs,x,y:ys) = (x:xs, y, ys) 
zFwd _           = error "zFwd: zipper ran out of forward list"
zBack (x:xs,y,ys) = (xs,x, y:ys) 
zBack _           = error "zBack: zipper ran out of backward list"
zInc,zDec :: Zipper Byte -> Zipper Byte
zInc (xs, a, ys) = (xs, a + 1, ys)
zDec (xs, a, ys) = (xs, a - 1, ys)
zPut :: a -> Zipper a -> Zipper a
zPut v (xs, _, ys) = (xs, v, ys) 
zGet ::  (t, t1, t2) -> t1
zGet (_, a, _) = a
--------------------------------------------------------------------------------

data World = World { wpc     :: Int
                   , pointer :: Int
                   , program :: Array Int Instruction
                   , memory  :: Zipper Byte
                   } deriving (Show)

initWorld :: [Instruction] -> World
initWorld cmds = 
  World { pointer = 0
        , memory  = mem
        , program = progAry
        , wpc     = 0
        }
 where 
   progAry = toInstructionArray cmds
   mem = (repeat 0, 0, repeat 0) -- infinite in both directions


main ::  IO ()
main = do
  [fname] <- getArgs
  _counts <- tests
  inp <- readFile fname
  parseAndRun inp

parseAndRun ::  String -> IO ()
parseAndRun str = 
  run (parse str) >> return ()

data Instruction = IncP
                 | DecP
                 | IncB
                 | DecB
                 | OutB
                 | InpB
                 | JmpF
                 | JmpB
                 deriving (Show, Enum, Eq, Ord)

toInstructionArray :: [Instruction] -> Array Int Instruction
toInstructionArray cmds = array (0, length cmds - 1) $ zip [0..] cmds

parseToArray ::  String -> Array Int Instruction
parseToArray = toInstructionArray . parse

parse :: String -> [Instruction]
parse cs = map parseOne $ filter legalInstructionChar cs

parseOne :: Char -> Instruction
parseOne '>' = IncP
parseOne '<' = DecP
parseOne '+' = IncB
parseOne '-' = DecB
parseOne '.' = OutB
parseOne ',' = InpB
parseOne '[' = JmpF
parseOne ']' = JmpB
parseOne other = error $ "illegal character: " ++ [other]

legalInstructionChar :: Char -> Bool
legalInstructionChar = flip elem "><+-.,[]"

display :: [Byte] -> String
display = map (chr . fromIntegral) . reverse

run :: [Instruction] -> IO World
run cmds = run' (initWorld cmds) 

run' :: World -> IO World
run' w = case nextInstruction w of
  Nothing    -> return w
  Just instr -> do
    next <- applyAndAdvance instr w
    run' next

nextInstruction :: World -> Maybe Instruction
nextInstruction w 
  | wpc w > lastInstrAddr = Nothing
  | otherwise             = Just (program w ! wpc w)
  where 
    lastInstrAddr = snd $ bounds $ program w

applyAndAdvance :: Instruction -> World -> IO World
applyAndAdvance instr w =
  fmap pcChange $ apply instr w
    where
      pcChange = if handlesPC instr then id else incPC
      -- does the instruction take care of PC for itself 
      -- or should it be advanced?
      handlesPC JmpF = True
      handlesPC JmpB = True
      handlesPC _    = False


modMem :: (Zipper Byte -> Zipper Byte) -> World -> World
modMem f w = w { memory = f $ memory w }

apply ::  Instruction -> World -> IO World
apply IncP w = return $ modMem zFwd w
apply DecP w = return $ modMem zBack w
apply IncB w = return $ modMem zInc w
apply DecB w = return $ modMem zDec w
apply OutB w = putChar (chr $ fromIntegral $ byteAtPointer w) >> return w
apply InpB w = do
  c <- getChar
  let newByte = fromIntegral $ ord c
  return $ modMem (zPut newByte) w
apply JmpF w = return $ if byteAtPointer w == 0 then jumpForward w else incPC w
apply JmpB w = return $ if byteAtPointer w /= 0 then jumpBackward w else incPC w

data JumpDir = Back | Forward deriving (Eq)

jumpBackward ::  World -> World
jumpBackward = modPC jb
jumpForward ::  World -> World
jumpForward = modPC jf

jf :: Int -> Array Int Instruction -> Int
jf pc arr = jump pc arr Forward

jb :: Int -> Array Int Instruction -> Int
jb pc arr = jump pc arr Back

jump :: Int -> Array Int Instruction -> JumpDir -> Int
jump progc arr dir = jf' progc 0
  where
    jf' :: Int -> Int -> Int
    jf' pc depth | byt == less  = if depth == 0 then pc'+1 else jf' pc' (depth-1)
                 | byt == more  = jf' pc' (depth + 1)
                 | otherwise    = jf' pc' depth
      where
        byt = arr ! pc' 
        pc' = pc + inc
    (less, more)           = lessAndMoreFor dir
    lessAndMoreFor Forward = (JmpB, JmpF)
    lessAndMoreFor Back    = (JmpF, JmpB)
    inc                    = pcIncFor dir
    pcIncFor Forward       = 1
    pcIncFor Back          = -1

incPC ::  World -> World
incPC w = w { wpc = wpc w + 1 }

modPC ::  (Int -> Array Int Instruction -> Int) -> World -> World
modPC f w = w { wpc = pc' }
  where pc' = f (wpc w) (program w)

byteAtPointer ::  World -> Byte
byteAtPointer w = zGet $ memory w 

tests ::  IO Counts
tests = runTestTT $ TestList [ jfTests, rolloverTests] {- , runnerTests]
runnerTests :: Test
runnerTests = TestList [ "no inp, no output" ~: []  ~=? (run $ parse "") 
                       , "single instr"      ~: [0] ~=? (run $ parse ".")
                       , "single instr"      ~: "Hello World!\n" ~=? (display $ run $ parse 
                      "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")
                       ]
-}

rolloverTests :: Test
rolloverTests = TestList  [ 127  ~=?  (subtract 1)  (-128) 
                          , 126  ~=?  (subtract 2)  (-128) 
                          , -128 ~=?  (subtract 0)  (-128) 
                          , -128 ~=?  (subtract 1)  (-127) 
                          , 127  ~=?  (+1)          126    
                          , 127  ~=?  (+0)          127    
                          , -128 ~=?  (+1)          127    
                          , -127 ~=?  (+2)          127    
                          ]
jfTests ::  Test
jfTests = TestList [ "jfsimple"           ~:  9 ~=? jf 1 simpleInstrs
                   , "jfnested all"       ~: 12 ~=? jf 1 nestedInstrs 
                   , "jfnested two layer" ~: 10 ~=? jf 3 nestedInstrs 
                   , "tiny f"             ~:  2 ~=? jf 0 tinyInstrs

                   , "tiny b"       ~: 1 ~=? jb 1 tinyInstrs
                   , "jbsimple"     ~: 2 ~=? jb 8 simpleInstrs
                   , "jbnested all" ~: 2 ~=? jb 11 nestedInstrs
                   , "jbnested two" ~: 4 ~=? jb 9 nestedInstrs
                   ]
  where 
    simpleInstrs = parseToArray "+[>>--++]-"
    nestedInstrs = parseToArray "+[>[>[-+]]-]+"
    tinyInstrs   = parseToArray "[]"


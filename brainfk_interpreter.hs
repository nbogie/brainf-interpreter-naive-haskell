module Main where
import Control.Monad.Trans.State(modify,execStateT,StateT,get,gets)
import Control.Monad.Trans(lift)
import Data.Array(array,(!),Array,bounds)
import Data.Char (chr, ord)
import Data.Int (Int8)
import System.Environment (getArgs)
import Test.HUnit hiding (State)

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
                   , output  :: [Byte]
                   } deriving (Show)

initWorld :: [Instruction] -> World
initWorld cmds = 
  World { pointer = 0
        , memory  = mem
        , program = progAry
        , wpc     = 0
        , output  = [] }
 where 
   progAry = toInstructionArray cmds
   mem = (repeat 0, 0, repeat 0) -- infinite in both directions

data Mode = ShowProg | RunProg deriving (Eq)

main ::  IO ()
main = do
  [fname] <- getArgs
  _counts <- tests
  readFile fname >>= parseRunAndDisplay >>= putStrLn

parseRunAndDisplay ::  String -> IO String
parseRunAndDisplay str = fmap display $ run (parse str)

parseAndShow ::  String -> String
parseAndShow = unlines . map show . zip [(0::Int)..] . parse

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

run :: [Instruction] -> IO [Byte]
run cmds = do
  let w  = initWorld cmds
  w' <- execStateT run' w
  return $ output w' 

run' :: StateT World IO ()
run' = do
  w <- get
  case nextInstruction w of
    Nothing    -> return ()
    Just instr -> applyAndAdvance instr >> run'

nextInstruction :: World -> Maybe Instruction
nextInstruction w 
  | wpc w > lastInstrAddr = Nothing
  | otherwise             = Just (program w ! wpc w)
  where 
    lastInstrAddr = snd $ bounds $ program w

applyAndAdvance :: Instruction -> StateT World IO ()
applyAndAdvance instr =  apply instr >> modify pcChange
  where
    pcChange = if handlesPC instr then id else incPC
    -- does the instruction take care of PC for itself or should it be advanced?
    handlesPC JmpF = True
    handlesPC JmpB = True
    handlesPC _ = False

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM action a b = do
  res <- action
  if res then a else b

(>>=<) ::  (Functor m) => m a -> (a -> b) -> m b
(>>=<) = flip fmap
-- useful for stuff like:  gets byteAtPointer >>=< (==0)

modMem :: (Zipper Byte -> Zipper Byte) -> World -> World
modMem f w = w { memory = f $ memory w }

byteToChar ::  Byte -> Char
byteToChar = chr . fromIntegral

charToByte :: Char -> Maybe Byte
charToByte c = if code <= 255 then Just $ fromIntegral code else Nothing
  where code = ord c

apply ::  Instruction -> StateT World IO ()
apply IncP = modify $ modMem zFwd
apply DecP = modify $ modMem zBack
apply IncB = modify $ modMem zInc
apply DecB = modify $ modMem zDec
apply OutB = do
  b <- gets byteAtPointer
  lift $ putChar $ byteToChar b
  
  -- Just for testing, useful if we still keep our output
  newVal <- fmap (b:) (gets output)
  modify (\s -> s { output = newVal })

apply InpB = do
  c <- lift getChar
  case charToByte c of
    Just b -> modify $ modMem (zPut b)
    Nothing -> error $ "Incompatible character read: "++ [c] 

apply JmpF = 
  ifM (gets byteAtPointer >>=< (==0) )
      (modify jumpForward)
      (modify incPC)
apply JmpB = 
    ifM (gets byteAtPointer >>=< (/= 0))
      (modify jumpBackward)
      (modify incPC)

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
tests = runTestTT $ TestList [ jfTests, rolloverTests, runnerTests]
runnerTests :: Test
runnerTests = TestList $ map runAndTest
  [ ("no in or out", "",   "")
  , ("single instr", "\0", ".")
  , ("single instr", "Hello World!\n", 
     "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")
                       ]
runAndTest :: (String, String, String) -> Test
runAndTest (msg, expd, inp) = TestCase $ do
  outp <- fmap display (run $ parse inp)
  assertEqual msg expd outp

rolloverTests :: Test
rolloverTests = TestList  [ 127  ~=?  (subtract 1)  (-128::Int8) 
                          , 126  ~=?  (subtract 2)  (-128::Int8) 
                          , -128 ~=?  (subtract 0)  (-128::Int8) 
                          , -128 ~=?  (subtract 1)  (-127::Int8) 
                          , 127  ~=?  (+1)          (126::Int8)   
                          , 127  ~=?  (+0)          (127::Int8)   
                          , -128 ~=?  (+1)          (127::Int8)   
                          , -127 ~=?  (+2)          (127::Int8)   
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


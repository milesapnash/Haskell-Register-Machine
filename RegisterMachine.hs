import Data.Array

data Instruction = I Int Int|
                   D Int Int Int|
                   H
    deriving (Eq)

instance Show Instruction where
    show H          = "HALT"
    show (I r l)    = "R" ++ show r ++ "+ -> L" ++ show l
    show (D r l l') = "R" ++ show r ++ "- -> L" ++ show l ++ ", L" ++ show l'

fromDoubleArrowPair :: Int -> Int -> Int
fromDoubleArrowPair x y = (2 ^ x) * (2 * y + 1)

fromSingleArrowPair :: Int -> Int -> Int
fromSingleArrowPair x y = ((2 ^ x) * (2 * y + 1)) - 1

toDoubleArrowPair :: Int -> (Int, Int)
toDoubleArrowPair p = (x, y)
  where
    x = multiplesOfTwo p
    y = ((p `div` (2 ^ x)) - 1) `div` 2

toSingleArrowPair :: Int -> (Int, Int)
toSingleArrowPair p = (x, y)
  where
    q = p + 1
    x = multiplesOfTwo q
    y = ((q `div` (2 ^ x)) - 1) `div` 2

multiplesOfTwo :: Int -> Int
multiplesOfTwo a
  | even a    = 1 + multiplesOfTwo (a `div` 2)
  | otherwise = 0

decodeInstruction :: Int -> Instruction
decodeInstruction 0 = H
decodeInstruction i
  | even x    = I (x `div` 2) y
  | otherwise = D ((x - 1) `div` 2) j k
    where
      (x, y) = toDoubleArrowPair i
      (j, k) = toSingleArrowPair y

decodeInt :: Int -> [Int]
decodeInt 0 = []
decodeInt l = x : decodeInt y
  where
    (x, y) = toDoubleArrowPair l

decodeList :: [Int] -> [Instruction]
decodeList = map decodeInstruction

decode :: Int -> [Instruction]
decode = decodeList . decodeInt

printDecode :: Int -> IO ()
printDecode i = mapM_ print (decode i)

increment :: Int -> Int -> [Int] -> [Int]
increment r l s = s''
  where
    (x, y:ys) = splitAt (r + 1) s
    s'        = x ++ [y + 1] ++ ys
    s''       = l : tail s'

decrement :: Int -> Int -> Int -> [Int] -> [Int]
decrement r l l' s
  | y > 0     = d''
  | otherwise = n
    where
      q@(x, y:ys) = splitAt (r + 1) s
      n           = l' : tail (x ++ (y:ys))
      d'          = x ++ [y - 1] ++ ys
      d''         = l : tail d'

halt :: [Int] -> [Int]
halt s = -1 : tail s

executeInstruction :: Instruction -> [Int] -> [Int]
executeInstruction H s          = halt s
executeInstruction (I r l) s    = increment r l s
executeInstruction (D r l l') s = decrement r l l' s

executeInstructions :: [Instruction] -> [Int] -> [Int]
executeInstructions instrs s
  | l > -1 && l < length instrs = executeInstructions instrs (executeInstruction x s)
  | otherwise                   = s
    where
      l = head s
      x = instrs !! l

execute :: Int -> [Int] -> [Int]
execute p = executeInstructions (decode p)

executeInstructionsTrace :: [Instruction] -> [Int] -> [(Instruction, [Int])]
executeInstructionsTrace instrs s
  | l > -1 && l < length instrs = (x, s) : executeInstructionsTrace instrs (executeInstruction x s)
  | otherwise                   = [(H, s)]
    where
      l = head s
      x = instrs !! l

executeTrace :: Int -> [Int] -> IO ()
executeTrace p s = mapM_ print (executeInstructionsTrace (decode p) s)
import PairFunctions
import Programs
import Data.List
    
encodeList :: [Int] -> Int
encodeList = foldr encodeDoublePair 0

decodeList :: Int -> [Int]
decodeList = unfoldr decoding
  where
    decoding 0 = Nothing
    decoding n = Just (decodeDoublePair n)

decodeInstructions :: [Int] -> [Instruction]
decodeInstructions = map decodeInstruction

decode :: Int -> [Instruction]
decode = decodeInstructions . decodeList

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

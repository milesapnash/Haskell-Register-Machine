import PairFunctions
import Programs
import Data.List
import Data.Array
    
encodeList :: [Integer] -> Integer
encodeList = foldr encodeDoublePair 0

decodeList :: Integer -> [Integer]
decodeList = unfoldr decoding
  where
    decoding 0 = Nothing
    decoding n = Just (decodeDoublePair n)

decodeInstructions :: [Integer] -> Program
decodeInstructions = fromInstructions . map decodeInstruction

decode :: Integer -> Program
decode = decodeInstructions . decodeList

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

executeInstructions :: Program -> [Int] -> [Int]
executeInstructions p@(Program xs) s
  | l > -1 && l < length xs = executeInstructions p (executeInstruction x s)
  | otherwise               = s
    where
      l = head s
      x = xs ! l

execute :: Integer -> [Int] -> [Int]
execute p = executeInstructions (decode p)

executeInstructionsTrace :: Program -> [Int] -> [(Instruction, [Int])]
executeInstructionsTrace p@(Program xs) s
  | l > -1 && l < length xs = (x, s) : executeInstructionsTrace p (executeInstruction x s)
  | otherwise               = [(H, s)]
    where
      l = head s
      x = xs ! l

executeTrace :: Integer -> [Int] -> IO ()
executeTrace p s = mapM_ print (executeInstructionsTrace d (checkState d s))
  where
    d = decode p

checkState :: Program -> [Int] -> [Int]
checkState p s 
 | length s > n = take n s
 | otherwise    = s
 where
  n = noRegisters p
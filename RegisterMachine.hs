module RegisterMachine where

import Pairing
import Programs
import Data.List (unfoldr)
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

encode :: Program -> Integer
encode = encodeList . map encodeInstruction . toInstructions

increment :: Int -> Int -> [Int] -> [Int]
increment r l s = l : tail (x ++ [y + 1] ++ ys)
  where (x, y:ys) = splitAt (r + 1) s

decrement :: Int -> Int -> Int -> [Int] -> [Int]
decrement r l l' s
  | y > 0     = l  : tail (x ++ [y - 1] ++ ys)
  | otherwise = l' : tail s
  where (x, y:ys) = splitAt (r + 1) s

halt :: [Int] -> [Int]
halt s = -1 : tail s

executeInstruction :: Instruction -> [Int] -> [Int]
executeInstruction H s          = halt s
executeInstruction (I r l) s    = increment r l s
executeInstruction (D r l l') s = decrement r l l' s

executeInstructions :: Program -> [Int] -> [Int]
executeInstructions p@(Program xs) s
  | inRange (bounds xs) l = executeInstructions p (executeInstruction x s)
  | otherwise             = s
    where
      l = head s
      x = xs ! l

execute :: Integer -> [Int] -> [Int]
execute p = executeInstructions (decode p)

executeInstructionsTrace :: Program -> [Int] -> [(Instruction, [Int])]
executeInstructionsTrace p@(Program xs) s
  | inRange (bounds xs) l = (x, s) : executeInstructionsTrace p (executeInstruction x s)
  | otherwise             = [(H, s)]
    where
      l = head s
      x = xs ! l

executeTrace :: Integer -> [Int] -> IO ()
executeTrace p s = mapM_ print (executeInstructionsTrace d (checkState d s))
  where
    d = decode p

checkState :: Program -> [Int] -> [Int]
checkState p s
  | len > n   = take n s
  | len < n   = s ++ replicate (n - len) 0
  | otherwise = s
  where
    n   = numRegisters p
    len = length s
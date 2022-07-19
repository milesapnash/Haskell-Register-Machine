module Programs where

import PairFunctions
import Data.Foldable
import Data.Array

data Instruction = H |
                   I Int Int |
                   D Int Int Int
  deriving Eq

instance Show Instruction where
    show H          = "HALT"
    show (I r l)    = "R" ++ show r ++ "+ -> L" ++ show l
    show (D r l l') = "R" ++ show r ++ "- -> L" ++ show l ++ ", L" ++ show l'

newtype Program = Program (Array Int Instruction)
  deriving Eq
  
instance Show Program where
  show (Program a)
    | null a    = "NULL"
    | otherwise = "PROGRAM:" ++ concatMap (\(i, l) -> "\nL" ++ show i ++ ":\t" ++ show l) (zip [0..] (toList a))

decodeInstruction :: Integer -> Instruction
decodeInstruction 0 = H
decodeInstruction i
  | even x    = I (x `div` 2) y
  | otherwise = D ((x - 1) `div` 2) j k
    where
      (x, y) = decodeDoublePair (fromIntegral i)
      (j, k) = decodeSinglePair y
      
encodeInstruction :: Instruction -> Integer
encodeInstruction H = 0
encodeInstruction (I r l)   = encodeDoublePair (2 * fromIntegral r) (fromIntegral l)
encodeInstruction (D r l l') = encodeDoublePair (2 * fromIntegral r + 1) (encodeSinglePair (fromIntegral l) (fromIntegral l'))

fromInstructions :: [Instruction] -> Program
fromInstructions is = Program (array bnds [(i, is !! i) | i <- range bnds])
  where
    bnds = (0, length is - 1)

noRegisters :: Program -> Int
noRegisters (Program p) = 2 + foldr max 0 (fmap registers p)
  where
    registers H = 0
    registers (I r _) = r
    registers (D r _ _) = r
module Programs where

import PairFunctions

data Instruction = I Int Int|
                   D Int Int Int|
                   H
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
    | otherwise = "PROGRAMMER"
   
decodeInstruction :: Int -> Instruction
decodeInstruction 0 = H
decodeInstruction i
  | even x    = I (x `div` 2) y
  | otherwise = D ((x - 1) `div` 2) j k
    where
      (x, y) = decodeDoublePair i
      (j, k) = decodeSinglePair y
      
encodeInstruction :: Instruction -> Int
encodeInstruction H = 0
encodeInstruction (I r l)   = encodeDoublePair (2 * fromIntegral r) (fromIntegral l)
encodeInstruction (D r l b) = encodeDoublePair (2 * fromIntegral r + 1) (encodeSinglePair (fromIntegral l) (fromIntegral b))

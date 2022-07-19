module PairFunctions where

encodeDoublePair :: (Integral a) => a -> a -> a
encodeDoublePair x y = (2 ^ x) * (2 * y + 1)

decodeDoublePair :: (Integral a) => a -> (a, a)
decodeDoublePair p
  | m == 1    = (0, fromIntegral d)
  | otherwise = (x + 1, y)
    where
      (d, m) = divMod p 2
      (x, y) = decodeDoublePair d

encodeSinglePair :: (Integral a) => a -> a -> a
encodeSinglePair x y = encodeDoublePair x y - 1
      
decodeSinglePair :: (Integral a) => a -> (a, a)
decodeSinglePair = decodeDoublePair . (+1)
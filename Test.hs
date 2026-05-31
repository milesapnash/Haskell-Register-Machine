module Main where

import Pairing
import Programs
import RegisterMachine
import System.Exit
import Test.QuickCheck

-- Manual tests

assert :: (Eq a, Show a) => String -> a -> a -> IO ()
assert name expected actual
  | expected == actual = putStrLn ("PASS: " ++ name)
  | otherwise = do
      putStrLn ("FAIL: " ++ name)
      putStrLn ("  expected: " ++ show expected)
      putStrLn ("  actual:   " ++ show actual)
      exitFailure

-- QuickCheck helpers

newtype SmallNat = SmallNat Integer deriving Show

instance Arbitrary SmallNat where
  arbitrary = SmallNat . toInteger <$> chooseInt (0, 16)
  shrink (SmallNat n) = [SmallNat n' | n' <- shrink n, n' >= 0]

instance Arbitrary Instruction where
  arbitrary = oneof
    [ pure H
    , I <$> small <*> small
    , D <$> small <*> small <*> small
    ]
    where small = abs <$> resize 10 arbitrary
  shrink H = []
  shrink (I r l) = H : [I r' l' | (r', l') <- shrink (r, l), r' >= 0, l' >= 0]
  shrink (D r l l') = H : [D r' k k' | (r', k, k') <- shrink (r, l, l'), r' >= 0, k >= 0, k' >= 0]

-- Properties

prop_doublePairRoundtrip :: SmallNat -> SmallNat -> Bool
prop_doublePairRoundtrip (SmallNat x) (SmallNat y) =
  decodeDoublePair (encodeDoublePair x y) == (x, y)

prop_singlePairRoundtrip :: SmallNat -> SmallNat -> Bool
prop_singlePairRoundtrip (SmallNat x) (SmallNat y) =
  decodeSinglePair (encodeSinglePair x y) == (x, y)

prop_instructionRoundtrip :: Instruction -> Bool
prop_instructionRoundtrip i =
  decodeInstruction (encodeInstruction i) == i

prop_listRoundtrip :: Property
prop_listRoundtrip = forAll (resize 32 arbitrary) $ \ns ->
  let xs = map (\(SmallNat n) -> n) ns
  in decodeList (encodeList xs) == xs

prop_programRoundtrip :: Property
prop_programRoundtrip = forAll (resize 32 (listOf1 arbitrary)) $ \is ->
  let p = fromInstructions is
  in decode (encode p) == p

-- Runner

check :: String -> Property -> IO ()
check name prop = do
  result <- quickCheckResult (withNumTests 200 prop)
  case result of
    Success {} -> putStrLn ("PASS: " ++ name)
    _          -> putStrLn ("FAIL: " ++ name) >> exitFailure

main :: IO ()
main = do
  -- Manual tests
  let prog = decode 786432
      expected = fromInstructions [D 0 0 2, H]
  assert "decode 786432" expected prog
  assert "encode roundtrip" 786432 (encode prog)

  -- QuickCheck properties
  check "doublePair roundtrip" (property prop_doublePairRoundtrip)
  check "singlePair roundtrip" (property prop_singlePairRoundtrip)
  check "instruction roundtrip" (property prop_instructionRoundtrip)
  check "list roundtrip" (property prop_listRoundtrip)
  check "program roundtrip" (property prop_programRoundtrip)

  putStrLn "All tests passed."

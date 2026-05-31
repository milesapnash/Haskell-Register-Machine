module Main where

import Programs
import RegisterMachine
import System.Exit

assert :: (Eq a, Show a) => String -> a -> a -> IO ()
assert name expected actual
  | expected == actual = putStrLn ("PASS: " ++ name)
  | otherwise = do
      putStrLn ("FAIL: " ++ name)
      putStrLn ("  expected: " ++ show expected)
      putStrLn ("  actual:   " ++ show actual)
      exitFailure

main :: IO ()
main = do
  let prog = decode 786432
      expected = fromInstructions [D 0 0 2, H]
  assert "decode 786432" expected prog
  assert "encode roundtrip" 786432 (encode prog)
  putStrLn "All tests passed."

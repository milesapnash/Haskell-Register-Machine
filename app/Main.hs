module Main where

import System.Environment
import System.Exit
import System.IO
import Text.Read (readMaybe)
import Parser
import Programs
import RegisterMachine

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("run" : rest)   -> parseAndRun rest False 1000000
    ["encode", file] -> encodeFile file
    ["decode", n]    -> case readMaybe n of
      Just num -> decodeNum num
      Nothing  -> die ("not a valid integer: " ++ n)
    _                -> usage

parseAndRun :: [String] -> Bool -> Int -> IO ()
parseAndRun ("--trace" : rest) _ ms     = parseAndRun rest True ms
parseAndRun ("--max-steps" : n : rest) t _ = case readMaybe n of
  Just ms -> parseAndRun rest t ms
  Nothing -> die ("--max-steps: not a valid integer: " ++ n)
parseAndRun (file : regs) trace maxSteps = case mapM readMaybe regs of
  Just rs -> run trace maxSteps file rs
  Nothing -> die "register values must be integers"
parseAndRun _ _ _                          = usage

die :: String -> IO a
die msg = hPutStrLn stderr ("error: " ++ msg) >> exitFailure

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage:"
  hPutStrLn stderr "  rm-sim run [--trace] [--max-steps N] <file> [r0 r1 ...]"
  hPutStrLn stderr "  rm-sim encode <file>"
  hPutStrLn stderr "  rm-sim decode <integer>"
  exitFailure

loadProgram :: FilePath -> IO Program
loadProgram file = do
  src <- readFile file
  case parseProgram src of
    Left err   -> hPutStrLn stderr err >> exitFailure
    Right prog -> return prog

run :: Bool -> Int -> FilePath -> [Int] -> IO ()
run trace maxSteps file regs = do
  prog <- loadProgram file
  let state = checkState prog (0 : regs)
  if trace
    then mapM_ print (take maxSteps (executeInstructionsTrace prog state))
    else do
      let (halted, result) = executeWithLimit maxSteps prog state
      putStrLn $ "Registers: " ++ show (tail result)
      if halted then return ()
      else hPutStrLn stderr $ "Warning: step limit (" ++ show maxSteps ++ ") reached, program may not have terminated"

encodeFile :: FilePath -> IO ()
encodeFile file = do
  prog <- loadProgram file
  print (encode prog)

decodeNum :: Integer -> IO ()
decodeNum = print . decode

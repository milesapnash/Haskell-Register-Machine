module Main where

import System.Environment
import System.Exit
import System.IO
import Parser
import Programs
import RegisterMachine

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("run" : "--trace" : file : regs) -> run True file (map read regs)
    ("run" : file : regs)             -> run False file (map read regs)
    ["encode", file]                  -> encodeFile file
    ["decode", n]                     -> decodeNum (read n)
    _                                 -> usage

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage:"
  hPutStrLn stderr "  rm-sim run [--trace] <file> [r0 r1 ...]"
  hPutStrLn stderr "  rm-sim encode <file>"
  hPutStrLn stderr "  rm-sim decode <integer>"
  exitFailure

loadProgram :: FilePath -> IO Program
loadProgram file = do
  src <- readFile file
  case parseProgram src of
    Left err   -> hPutStrLn stderr err >> exitFailure
    Right prog -> return prog

run :: Bool -> FilePath -> [Int] -> IO ()
run trace file regs = do
  prog <- loadProgram file
  let state = checkState prog (0 : regs)
  if trace
    then mapM_ print (executeInstructionsTrace prog state)
    else putStrLn $ "Registers: " ++ show (tail (executeInstructions prog state))

encodeFile :: FilePath -> IO ()
encodeFile file = do
  prog <- loadProgram file
  print (encode prog)

decodeNum :: Integer -> IO ()
decodeNum = print . decode

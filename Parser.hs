module Parser (parseInstruction, parseProgram) where

import Programs
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

nat :: ReadP Int
nat = read <$> munch1 isDigit

ws :: ReadP ()
ws = () <$ munch (\c -> c == ' ' || c == '\t')

instruction :: ReadP Instruction
instruction = halt +++ inc +++ dec
  where
    halt = H <$ string "HALT"
    inc = do
      _ <- char 'R'
      r <- nat
      _ <- string "+ -> L"
      l <- nat
      return (I r l)
    dec = do
      _ <- char 'R'
      r <- nat
      _ <- string "- -> L"
      l <- nat
      _ <- string ", L"
      l' <- nat
      return (D r l l')

labelledLine :: ReadP Instruction
labelledLine = do
  _ <- char 'L'
  _ <- nat
  _ <- char ':'
  ws
  instruction

runParser :: ReadP a -> String -> Either String a
runParser p s = case readP_to_S (p <* eof) s of
  [(x, "")] -> Right x
  _         -> Left ("parse error: " ++ s)

parseInstruction :: String -> Either String Instruction
parseInstruction = runParser instruction

parseProgram :: String -> Either String Program
parseProgram "NULL" = Right (fromInstructions [])
parseProgram s = fmap fromInstructions (mapM (runParser labelledLine) ls)
  where
    ls = filter (not . null) $ case lines s of
      ("PROGRAM:" : rest) -> rest
      other               -> other

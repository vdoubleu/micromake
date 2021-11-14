module Lib
    ( wordsWhen,
      exit,
    ) where

import System.Exit

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where (w, s'') = break p s'

exit = exitWith ExitSuccess

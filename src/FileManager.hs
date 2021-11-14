module FileManager
    (
      inCppFolder,
      getCurrDirFiles,
      cFiles,
      cppFiles
    ) where

import System.Directory
import Lib

inCppFolder :: IO(Bool)
inCppFolder = do
  fp <- getCurrentDirectory
  c <- getDirectoryContents fp
  let currDirContainsCppFile = containsCppFile c
      currDirContainsCFile   = containsCFile c
    in
      if currDirContainsCFile && currDirContainsCppFile then do 
        putStrLn "Current directory contains both c and cpp files, don't know which language to compile for"
        exit
      else if not currDirContainsCFile && not currDirContainsCppFile then do
          putStrLn "Current directory contains neither c or cpp files, don't know which language to compile for"
          exit
      else
        return currDirContainsCppFile

getCurrDirFiles :: IO([String])
getCurrDirFiles = do
  fp <- getCurrentDirectory
  c <- getDirectoryContents fp
  return c

cFiles :: [String] -> [String]
cFiles c = filter isCFile c


cppFiles :: [String] -> [String]
cppFiles c = filter isCppFile c


containsCppFile = any isCppFile
containsCFile = any isCFile

isCppFile :: String -> Bool
isCppFile s = last4 == ".cpp" || (drop 1 last4) == ".cc"
  where last4 = (lastN 4 s)

isCFile :: String -> Bool
isCFile s = last2 == ".c"
  where last2 = (lastN 2 s)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

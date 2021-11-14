module Main where

import System.Environment
import System.IO
import System.Process
import Data.Char
import Lib
import FlagProcessing
import FileManager

main = getArgs >>= parse >>= micromake

parse [] = do 
  putStrLn "---Running Stored Commands---"
  runCommands
parse a = 
  if (containsFlag a "help") then
    putStrLn micromakeHelpMsg >> exit
  else if (containsFlag a "version") then
    putStrLn "MicroMake Version 0.01" >> exit
  else if (containsFlag a "allCurrentFolder") then
    do 
      putStrLn "---Saving Compile All In Current Dir---"
      saveRunAllCmd
  else
    do 
      putStrLn "---Storing Commands---"
      saveCommands a 
      putStrLn "---Done---"
      exit

micromake :: String -> IO()
micromake s = do 
  putStr s
  putStrLn "---Done---"

saveRunAllCmd :: IO(String)
saveRunAllCmd = do
  i <- inCppFolder 
  currDirFiles <- getCurrDirFiles
  if i then
    writeFile "MicroMake" 
      (unlines 
        [
          "g++ " ++ (concat (cppFiles currDirFiles)),
          "./a.out"
        ]
      )
  else
    writeFile "MicroMake" 
      (unlines
        [
          "gcc " ++ (concat (cFiles currDirFiles)),
          "./a.out"
        ]
      )
  return ""

runCommands :: IO(String)
runCommands = do
  contents <- readFile "MicroMake"
  s <- (mapM runEachCmd (lines contents))
  return (unlines s)

saveCommands :: [String] -> IO(String)
saveCommands s = do 
  writeFile "MicroMake" unlinedStr
  displayWrittenCommands s
  return ""
    where unlinedStr = (unlines s)

runEachCmd :: String -> IO(String)
runEachCmd entireStr = readProcess cmd args []
  where (cmd:args) = wordsWhen (==' ') entireStr

displayWrittenCommands :: [String] -> IO()
displayWrittenCommands [] = return ()
displayWrittenCommands (s:ss) = do
  putStrLn s
  displayWrittenCommands ss


micromakeHelpMsg = 
  "MicroMake Help\nMicromake is designed to be an easy to use \nand ultra light weight version of make, \ndesigned to help you quickly run and build \nsmall programs that are too small to \nrequire you to need the full power of make" 
    ++ "\n\nTo use micromake, starting by running \'micromake\' (or just \'mm\') followed by the build commands: \nmm \"<build command 1>\" \"<build command 2>\" \"<build command ...>\""
    ++ "\n\nThis will create the MicoMake file which stores the build commands, \nthen run \'mm\' or \'micromake\' again in the same directory to run them."
  

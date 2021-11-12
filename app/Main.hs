module Main where

import System.Environment
import System.Exit
import System.IO
import System.Process
import Data.Char
import Lib

main = getArgs >>= parse >>= minimake

parse [] = do 
  putStrLn "---Running Stored Commands---"
  runCommands
parse s = 
  if (containsHelp s) then
    putStrLn minimakeHelpMsg >> exit
  else if (containsVers s) then
    putStrLn "MiniMake Version 0.01" >> exit
  else
    do 
      putStrLn "---Storing Commands---"
      saveCommands s 
      putStrLn "---Done---"
      exit

exit = exitWith ExitSuccess

minimake :: String -> IO()
minimake s = do 
  putStr s
  putStrLn "---Done---"

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

containsHelp :: [String] -> Bool
containsHelp strs = ("-h" `elem` strs) || ("-help" `elem` strs)

containsVers :: [String] -> Bool
containsVers strs = ("-v" `elem` strs) || ("-version" `elem` strs)

minimakeHelpMsg = 
  "MiniMake Help\nMinimake is designed to be an easy to use \nand ultra light weight version of make, \ndesigned to help you quickly run and build \nsmall programs that are too small to \nrequire you to need the full power of make" 
    ++ "\n\nTo use micromake, starting by running \'micromake\' (or just \'mm\') followed by the build commands: \nmm \"<build command 1>\" \"<build command 2>\" \"<build command ...>\""
    ++ "\n\nThis will create the MicoMake file which stores the build commands, \nthen run \'mm\' or \'micromake\' again in the same directory to run them."
  

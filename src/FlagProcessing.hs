module FlagProcessing
    ( containsFlag
    ) where

import Data.HashMap.Strict

containsFlag :: [String] -> String -> Bool
containsFlag flags desiredFlagType = any (`elem` flags) flagsToCheck
  where flagsToCheck = flagTypeToValidFlagsMap ! desiredFlagType

flagTypeToValidFlagsMap = fromList 
  [
    ("help", ["-h", "-help"]),
    ("version", ["-v", "-version"]),
    ("allCurrentFolder", ["-a", "-allInCurrentDir"])
  ]

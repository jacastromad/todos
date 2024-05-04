{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless, when)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Version (showVersion)
import Paths_todos (version)
import Source (projectFiles, todosInFile, todosInProject)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import Text.Read (readMaybe)

-- Command-line arguments
data Args = Args
  { number :: Int,
    priority :: Int,
    dir :: String,
    ignore :: [String],
    needHelp :: Bool,
    badArg :: String
  }
  deriving (Show)

-- Default arguments: All TODO comments (0), any priority (0), current dir (".")
-- and no help (False)
defArgs :: Args
defArgs = Args 0 0 "." [] False ""

-- Parse command line arguments
parseArgs :: [String] -> Args
parseArgs = parse defArgs
  where
    parse args [] = args
    parse args ("-h" : _) = args {needHelp = True}
    parse args ("-n" : n : rest) =
      case readMaybe n of
        Just num -> parse (args {number = num}) rest
        Nothing -> args {badArg = "No number after the -n flag"}
    parse args ("-p" : n : rest) =
      case readMaybe n of
        Just num -> parse (args {priority = num}) rest
        Nothing -> args {badArg = "No number after the -p flag"}
    parse args ("-i" : d : rest) = parse (args {ignore = splitOn "," d}) rest
    parse args [directory] = args {dir = directory}
    parse args _ = args {badArg = "Invalid arguments provided"}

-- Print help
printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ progName ++ " - version: " ++ showVersion version ++ "\n"
  putStrLn "Find and print TODO comments in a project."
  putStrLn "\nUsage:\n"
  putStrLn $ "  " ++ progName ++ " [OPTIONS]..." ++ " [DIRECTORY]\n"
  putStrLn "Flags:"
  putStrLn "  -n N          Print only the first N TODO comments"
  putStrLn "  -p P          Print TODO comments with priority >= N"
  putStrLn "  -i s1,s2,...  Ignore filepaths containing strings s1, s2, ..."
  putStrLn "  -h            Help\n"

-- Take n if n > 0, otherwise take all
takeN :: Int -> [a] -> [a]
takeN n xs
  | n > 0 = take n xs
  | otherwise = xs

main :: IO ()
main = do
  args <- getArgs
  let opt = parseArgs args

  when (needHelp opt) $ do
    printHelp
    exitSuccess

  unless (null $ badArg opt) $ do
    putStrLn $ "Error: " ++ badArg opt ++ "\n"
    printHelp
    exitFailure

  dirExists <- doesDirectoryExist $ dir opt
  unless dirExists $ do
    putStrLn $ "Error: directory " ++ dir opt ++ " does not exist\n"
    printHelp
    exitFailure

  unless (null $ ignore opt) $ do
    putStrLn "Ignoring filepaths containing any of the following strings:"
    putStrLn $ intercalate ", " (ignore opt) ++ "\n"
  todos <- todosInProject (priority opt) (dir opt) (ignore opt)
  mapM_ (\todo -> print todo >> putStrLn "") $ takeN (number opt) todos
  putStrLn $ show (length todos) ++ " TODO comments found."

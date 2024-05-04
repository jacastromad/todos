{-# LANGUAGE OverloadedStrings #-}

module Source
  ( langs,
    projectFiles,
    todosInFile,
    todosInProject,
  )
where

import Control.Monad (filterM, forM)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (isInfixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Todos (TODO (..), findTodos, sortTodos)

-- List of file extensions for different languages and their comment regex
langs :: Map String ByteString
langs =
  Map.fromList
    [ (".hs", "(?i)--[ \t]*TODO[ :].*$|\\{-[ \t\n]*TODO[ :](?:[^-]|-[^}])*-\\}"),
      (".c", "(?i)\\/\\/[ \t]*TODO[ :].*$|\\/\\*[ \t\n]*TODO[ :](?:[^\\*]|\\*[^\\/])*\\*\\/"),
      (".cpp", "(?i)\\/\\/[ \t]*TODO[ :].*$|\\/\\*[ \t\n]*TODO[ :](?:[^\\*]|\\*[^\\/])*\\*\\/"),
      (".go", "(?i)\\/\\/[ \t]*TODO[ :].*$|\\/\\*[ \t\n]*TODO[ :](?:[^\\*]|\\*[^\\/])*\\*\\/"),
      (".java", "(?i)\\/\\/[ \t]*TODO[ :].*$|\\/\\*[ \t\n]*TODO[ :](?:[^\\*]|\\*[^\\/])*\\*\\/"),
      (".kt", "(?i)\\/\\/[ \t]*TODO[ :].*$|\\/\\*[ \t\n]*TODO[ :](?:[^\\*]|\\*[^\\/])*\\*\\/"),
      (".rs", "(?i)\\/\\/[ \t]*TODO[ :].*$|\\/\\*[ \t\n]*TODO[ :](?:[^\\*]|\\*[^\\/])*\\*\\/"),
      (".cs", "(?i)\\/\\/[ \t]*TODO[ :].*$|\\/\\*[ \t\n]*TODO[ :](?:[^\\*]|\\*[^\\/])*\\*\\/"),
      (".js", "(?i)\\/\\/[ \t]*TODO[ :].*$|\\/\\*[ \t\n]*TODO[ :](?:[^\\*]|\\*[^\\/])*\\*\\/"),
      (".py", "(?i)#[ \t]*TODO[ :].*$"),
      (".lua", "(?i)--[ \t]*TODO[ :].*$|--\\[\\[[ \t\n]*TODO[ :](?:[^\\]]|\\][^\\]])*\\]\\]"),
      (".sql", "(?i)--[ \t]*TODO[ :].*$|\\/\\*[ \t\n]*TODO[ :](?:[^\\*]|\\*[^\\/])*\\*\\/"),
      (".asm", "(?i);[ \t]*TODO[ :].*$")
    ]

-- Return the list of files in the rootDir directory and its subdirectories
projectFiles :: FilePath -> IO [FilePath]
projectFiles rootDir = do
  contents <- listDirectory rootDir
  let paths = map (rootDir </>) contents
  files <- filterM doesFileExist paths
  directories <- filterM doesDirectoryExist paths
  filesInSubdirs <- mapM projectFiles directories
  return $ files ++ concat filesInSubdirs

-- Read filename source code and return a list of TODO comments found
-- If file extension is unknown (not in langs), return []
todosInFile :: FilePath -> IO [TODO]
todosInFile filename = do
  let ext = takeExtension filename
  case Map.lookup ext langs of
    Nothing -> return []
    Just regex -> do
      source <- B.readFile filename
      return $ findTodos filename regex source

-- Return TODO comments with priority >= prior found in a project
-- path is the root directory of the project
todosInProject :: Int -> FilePath -> [String] -> IO [TODO]
todosInProject prior path ignore = do
  files <- projectFiles path
  todos <- mapM todosInFile $ filter (\s -> not $ any (`isInfixOf` s) ignore) files
  return $ sortTodos $ filter (\t -> priority t >= prior) (concat todos)

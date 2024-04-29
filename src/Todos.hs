module Todos (
    TODO(..),
    findTodos,
    sortTodos
) where

import Text.Regex.PCRE
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T


-- A TODO comment
data TODO = TODO
    { filePath  :: FilePath    -- File where the comment was found
    , line      :: Int         -- Line number of the comment
    , comment   :: ByteString  -- The comment
    , priority  :: Int         -- Priority (number of '!' found in comment)
    } deriving (Eq)

instance Show TODO where
    show (TODO f l c p) = "File: " ++ f ++
                          "\nLine: " ++ show l ++
                          "\nPriority: " ++ show p ++
                          "\nComment:\n" ++ T.unpack (decodeUtf8 c)


-- Count the number of occurrences of char in text
count :: Char -> ByteString -> Int
count char text = B.length $ B.filter (== char) text


-- Find first match of regex in text
-- Return line number of match, match, priority (number of '!' in comment) and
-- text after the match
find :: ByteString -> ByteString -> (Int, ByteString, Int, ByteString)  -- (line, match, priority, post)
find regex text = (line, match, priority, post)
    where
        (prev, match, post) = text =~ regex
        line = count '\n' prev
        priority = count '!' match


-- Helper function
-- Find all TODO comments keeping the number of seen lines in the acc
-- Match regex in text, and set each TODO filePath to file
findTodos' :: Int -> FilePath -> ByteString -> ByteString -> [TODO]
findTodos' acc file regex text
    | B.null post = []
    | otherwise = TODO file line match priority : findTodos' nacc file regex post
    where
        (nline, match, priority, post) = find regex text
        line = nline+acc
        nacc = line + count '\n' match


-- Find all TODO comments in a Text
-- Start line numbers at 1
findTodos :: FilePath -> ByteString -> ByteString -> [TODO]
findTodos = findTodos' 1


-- Sort a list of TODO comments by priority (higher priority first)
sortTodos :: [TODO] -> [TODO]
sortTodos = sortOn $ Down . priority



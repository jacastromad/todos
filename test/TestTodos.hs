{-# LANGUAGE OverloadedStrings #-}

module TestTodos where

import Test.HUnit
import Control.Monad (when)
import Todos (TODO(..), findTodos, sortTodos)


text = " A!!! B C! \nD E! "
regex = "[A-Z][^A-Z \t\n]*"
todos = [TODO "file" 1 "A!!!" 3,
         TODO "file" 1 "B" 0,
         TODO "file" 1 "C!" 1,
         TODO "file" 2 "D" 0,
         TODO "file" 2 "E!" 1]

sorted = [TODO "file" 1 "A!!!" 3,
          TODO "file" 1 "C!" 1,
          TODO "file" 2 "E!" 1,
          TODO "file" 1 "B" 0,
          TODO "file" 2 "D" 0]


testFindTodos :: Test
testFindTodos = TestCase $ assertEqual "findTodos" todos (findTodos "file" regex text)


testSortTodos :: Test
testSortTodos = TestCase $ assertEqual "sortTodos" sorted (sortTodos todos)


tests :: Test
tests = TestList [testFindTodos, testSortTodos]



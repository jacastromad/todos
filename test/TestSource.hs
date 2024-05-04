{-# LANGUAGE OverloadedStrings #-}

module TestSource (tests) where

import Control.Monad (when)
import Source (todosInFile, todosInProject)
import Test.HUnit
import Todos (TODO (..), findTodos, sortTodos)

-- List returned by todosInFile is not sorted by priority
inFiles :: [(String, [TODO])]
inFiles =
  [ ( "./test/TestData/Haskell/Main.hs",
      [ TODO "./test/TestData/Haskell/Main.hs" 2 "-- TODO: a!" 1,
        TODO "./test/TestData/Haskell/Main.hs" 5 "--TODO: b" 0,
        TODO "./test/TestData/Haskell/Main.hs" 7 "--\tTODO c" 0,
        TODO "./test/TestData/Haskell/Main.hs" 9 "{-    TODO d!!!!! -}" 5,
        TODO "./test/TestData/Haskell/Main.hs" 13 "{-\n TODO e \n -\n - -}" 0,
        TODO "./test/TestData/Haskell/Main.hs" 18 "-- TODO f -- !!" 2,
        TODO "./test/TestData/Haskell/Main.hs" 19 "-- todo g !!" 2,
        TODO "./test/TestData/Haskell/Main.hs" 22 "{- TODO: h!\n -\n -\n-}" 1
      ]
    ),
    ( "./test/TestData/C/main.c",
      [ TODO "./test/TestData/C/main.c" 4 "/*    TODO: a!!!!!!!!! */" 9,
        TODO "./test/TestData/C/main.c" 5 "//TODO: b!" 1,
        TODO "./test/TestData/C/main.c" 6 "// TODO c!" 1,
        TODO "./test/TestData/C/main.c" 9 "/* TODO:\n     d\n     */" 0,
        TODO "./test/TestData/C/main.c" 16 "/*\n\nTODO:\ne!!\n\n*/" 2,
        TODO "./test/TestData/C/main.c" 23 "// todo f" 0
      ]
    ),
    ( "./test/TestData/Python/main.py",
      [ TODO "./test/TestData/Python/main.py" 2 "# TODO: a !!!!" 4,
        TODO "./test/TestData/Python/main.py" 5 "#\tTODO b" 0,
        TODO "./test/TestData/Python/main.py" 8 "#TODO: c  !!!" 3,
        TODO "./test/TestData/Python/main.py" 10 "#    TODO d" 0,
        TODO "./test/TestData/Python/main.py" 15 "# todo: e" 0
      ]
    ),
    ( "./test/TestData/SQL/query.sql",
      [ TODO "./test/TestData/SQL/query.sql" 3 "-- TODO: a  !!!" 3,
        TODO "./test/TestData/SQL/query.sql" 5 "/*\n\tTODO b\n*/" 0,
        TODO "./test/TestData/SQL/query.sql" 12 "--todo c" 0
      ]
    ),
    ( "./test/TestData/Lua/main.lua",
      [ TODO "./test/TestData/Lua/main.lua" 2 "--TODO: a" 0,
        TODO "./test/TestData/Lua/main.lua" 5 "--[[TODO b!!\n--\n--]]" 2,
        TODO "./test/TestData/Lua/main.lua" 9 "--\tTodo c" 0,
        TODO "./test/TestData/Lua/main.lua" 13 "--[[\nTODO: d\n--]]" 0,
        TODO "./test/TestData/Lua/main.lua" 17 "--    TODO: e!" 1
      ]
    ),
    ( "./test/TestData/asm/main.asm",
      [ TODO "./test/TestData/asm/main.asm" 2 ";TODO: a" 0,
        TODO "./test/TestData/asm/main.asm" 5 "; TODO b" 0,
        TODO "./test/TestData/asm/main.asm" 13 ";\tTodo !!! c" 3,
        TODO "./test/TestData/asm/main.asm" 15 ";    TODO: d" 0,
        TODO "./test/TestData/asm/main.asm" 19 "; TODO: e" 0,
        TODO "./test/TestData/asm/main.asm" 31 "; todo f" 0
      ]
    )
  ]

testTodosInFile :: Test
testTodosInFile = TestList $ map createTest inFiles
  where
    createTest (filename, expected) = TestCase $ do
      found <- todosInFile filename
      assertEqual filename expected found

-- List returned by todosInProject is must be sorted by priority
inProject :: [(Int, String, [TODO])]
inProject =
  [ ( 0,
      "./test/TestData/Haskell",
      [ TODO "./test/TestData/Haskell/Main.hs" 9 "{-    TODO d!!!!! -}" 5,
        TODO "./test/TestData/Haskell/Main.hs" 18 "-- TODO f -- !!" 2,
        TODO "./test/TestData/Haskell/Main.hs" 19 "-- todo g !!" 2,
        TODO "./test/TestData/Haskell/Main.hs" 2 "-- TODO: a!" 1,
        TODO "./test/TestData/Haskell/Main.hs" 22 "{- TODO: h!\n -\n -\n-}" 1,
        TODO "./test/TestData/Haskell/Main.hs" 5 "--TODO: b" 0,
        TODO "./test/TestData/Haskell/Main.hs" 7 "--\tTODO c" 0,
        TODO "./test/TestData/Haskell/Main.hs" 13 "{-\n TODO e \n -\n - -}" 0
      ]
    ),
    ( 1,
      "./test/TestData/Haskell",
      [ TODO "./test/TestData/Haskell/Main.hs" 9 "{-    TODO d!!!!! -}" 5,
        TODO "./test/TestData/Haskell/Main.hs" 18 "-- TODO f -- !!" 2,
        TODO "./test/TestData/Haskell/Main.hs" 19 "-- todo g !!" 2,
        TODO "./test/TestData/Haskell/Main.hs" 2 "-- TODO: a!" 1,
        TODO "./test/TestData/Haskell/Main.hs" 22 "{- TODO: h!\n -\n -\n-}" 1
      ]
    ),
    ( 2,
      "./test/TestData/Haskell",
      [ TODO "./test/TestData/Haskell/Main.hs" 9 "{-    TODO d!!!!! -}" 5,
        TODO "./test/TestData/Haskell/Main.hs" 18 "-- TODO f -- !!" 2,
        TODO "./test/TestData/Haskell/Main.hs" 19 "-- todo g !!" 2
      ]
    ),
    ( 4,
      "./test/TestData/Haskell",
      [TODO "./test/TestData/Haskell/Main.hs" 9 "{-    TODO d!!!!! -}" 5]
    ),
    ( 1,
      "./test/TestData/C/",
      [ TODO "./test/TestData/C/main.c" 4 "/*    TODO: a!!!!!!!!! */" 9,
        TODO "./test/TestData/C/main.c" 16 "/*\n\nTODO:\ne!!\n\n*/" 2,
        TODO "./test/TestData/C/main.c" 5 "//TODO: b!" 1,
        TODO "./test/TestData/C/main.c" 6 "// TODO c!" 1
      ]
    ),
    ( 3,
      "./test/TestData/Python",
      [ TODO "./test/TestData/Python/main.py" 2 "# TODO: a !!!!" 4,
        TODO "./test/TestData/Python/main.py" 8 "#TODO: c  !!!" 3
      ]
    ),
    ( 2,
      "./test/TestData/SQL/",
      [TODO "./test/TestData/SQL/query.sql" 3 "-- TODO: a  !!!" 3]
    ),
    ( 1,
      "./test/TestData/Lua/",
      [ TODO "./test/TestData/Lua/main.lua" 5 "--[[TODO b!!\n--\n--]]" 2,
        TODO "./test/TestData/Lua/main.lua" 17 "--    TODO: e!" 1
      ]
    ),
    ( 3,
      "./test/TestData/asm",
      [TODO "./test/TestData/asm/main.asm" 13 ";\tTodo !!! c" 3]
    )
  ]

testTodosInProject :: Test
testTodosInProject = TestList $ map createTest inProject
  where
    createTest (priority, root, expected) = TestCase $ do
      found <- todosInProject priority root []
      assertEqual root expected found

tests :: Test
tests =
  TestList
    [ TestLabel "todosInFile" testTodosInFile,
      TestLabel "todosInProject" testTodosInProject
    ]

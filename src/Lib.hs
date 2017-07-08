module Lib
    ( grid
    , languages
    , formatGrid
    , outputGrid
    , findWord
    , findWords
    , findWordInLine
	) where

import Data.List (isInfixOf)
import Data.Maybe (catMaybes)

type Grid = [String]

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

findWords :: Grid -> [String] -> [String]
findWords grid words = catMaybes (map (findWord grid) words)

findWord :: Grid -> String -> Maybe String 
findWord grid word =
	let lines = grid ++ map reverse grid 
	    found = or $ map (findWordInLine word) lines 
	in if found then Just word else Nothing

findWordInLine :: String -> String -> Bool
findWordInLine word line = word `isInfixOf` line

grid = [  "__C_________R__"
		, "__SI________U__"
		, "__HASKELL____B_"
		, "__A__A_____S__Y"
		, "__R___B___C____"
		, "__PHP____H_____"
		, "____S_LREP_____"
		, "____I__M_Y__L__"
		, "____L_E__T_O___"
		, "_________HB____"
		, "_________O_____"
		, "________CN_____"
		]

languages = [  "BASIC"
			,  "COBOL"
			,  "CSHARP"
			,  "HASKELL"
			,  "LISP"
			,  "PERL"
			,  "PHP"
			,  "PYTHON"
			,  "RUBY"
			,  "SCHEME"
			]
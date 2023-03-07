{-|
Module      : Markdown
Description : Very simple Markdown markup generation.
Stability   : stable
-}

module Markdown where

import Data.List.Extra

-- Headers

h1 :: String -> String
h1 s = "\n# " ++ s ++ "\n"

h2 :: String -> String
h2 s = "\n## " ++ s ++ "\n"

h3 :: String -> String
h3 s = "\n### " ++ s ++ "\n"

h4 :: String -> String
h4 s = "\n#### " ++ s ++ "\n"

h5 :: String -> String
h5 s = "\n##### " ++ s ++ "\n"

h6 :: String -> String
h6 s = "\n###### " ++ s ++ "\n"

-- Emphasis

bold :: String -> String
bold s = " **" ++ s ++ "** "

italics :: String -> String
italics s = " _" ++ s ++ "_ "

strikethrough :: String -> String
strikethrough s = " ~~" ++ s ++ "~~ "

-- Lists

olist :: Int -> [String] -> String
olist n ss = 
  "\n" ++ unlines (map (\(n,s) -> concat (replicate n "  ") ++ show n ++ ". " ++ s) ([1..] `zip` ss))

ulist :: Int -> [String] -> String
ulist n ss = 
  "\n" ++ unlines (map (\s -> concat (replicate n "  ") ++ "- " ++ s) ss)

-- Link

link :: String -> String -> String
link txt url = " [" ++ txt ++ "]" ++ "(" ++ url ++ ") "

-- Images

img :: String -> String -> String
img alt url = " ![" ++ alt ++ "]" ++ "(" ++ url ++ ") "

-- Code and Syntax Highlighting

code :: String -> String
code s = " `" ++ s ++ "` "

codeblock :: String -> String -> String
codeblock code lang = unlines ["\n```" ++ lang, code, "```"]

table :: [String] -> [[String]] -> String
table h rs = "\n" ++ header h ++ unlines (map row rs)
  where 
    header :: [String] -> String
    header cs = unlines [
      row cs,
      row (replicate (length cs) "---")
      ]

    row :: [String] -> String
    row cs = intercalate " | " (map cell cs)

    cell :: String -> String
    cell = replace "|" "\\|"

-- Blockquotes

quote :: String -> String
quote s = "\n> " ++ s ++ "\n"

-- Horizontal Rule

rule :: String
rule = "\n---\n"

-- Paragraph

par :: String -> String
par s = "\n\n" ++ s ++ "\n\n"  
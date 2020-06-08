module Main where

import Data.Char(toUpper)
import System.Environment
import Data.List

import Text.Regex.Posix
import Text.Regex
import Codec.Binary.UTF8.String

main :: IO ()
main = do
    args <- getArgs
    inpStr <- readFile (head args)
    writeFile "output.txt" $ applicatePerLines inpStr

applicatePerLines :: String -> String
applicatePerLines input =
    let allLines = lines input
        shortLines = map converter allLines
        result = unlines shortLines
    in  result

converter :: String -> String
converter x = (liC . headC . andC . strongC . mathC . linkC) x

headC :: String -> String
headC str = unlines $ map heads (lines str)
  where
    heads x
      | isPrefixOf "####" x  = "[*" ++ (drop 4 x) ++ " ]"
      | isPrefixOf "###" x  = "[**" ++ (drop 3 x) ++ " ]"
      | isPrefixOf "##" x  = "[***" ++ (drop 2 x) ++ " ]"
      | isPrefixOf "#" x  = "[****" ++ (drop 1 x) ++ " ]"
      | otherwise = x

andC :: String -> String
andC x =
    let
        gl x = gsub "&larr;" "←" x
        gr x = gsub "&rarr;" "→" x
    in (gl . gr) x


strongC :: String -> String
strongC str = unwords $ map tag ch
    where
        tag x
          | (encodeString x) =~ "\\*\\*.*\\*\\*.?" = "[* " ++ (reverse $ drop 2 $ reverse $ drop 2 x) ++ " ]"
          | otherwise = x
        ch = wordsWhen (==' ') str

linkC :: String -> String
linkC str = unwords $ map tag ch
    where
        tag x
          | (encodeString x) =~ "\\[.*\\]\\(.*\\)" = "[" ++ (init $ tail $ gsub "](" " " x) ++ "]"
          | otherwise = x
        ch = wordsWhen (==' ') str

mathC :: String -> String
mathC str = unwords $ map tag ch
    where
        tag x
          | (encodeString x) =~ "^\\$[^\\$]*\\$$" = "[$ " ++ (init . tail . rep) x ++ "]"
          | otherwise = x
        ch = wordsWhen (==' ') str
        rep = map (\c -> if c=='$' then ' ' else c)


liC :: String -> String
liC str = unlines $ map heads (lines str)
  where
    heads x
      | (encodeString x) =~ " *- .*"  = replaceX x '-' ' '
      | otherwise = x

mapOnce _ []     = []
mapOnce f (x:xs) = case f x of
        Nothing -> x : mapOnce f xs
        Just y  -> y : xs

replaceX items old new = mapOnce check items where
    check item  | item == old = Just new
                | otherwise   = Nothing

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

gsub _ _ [] = []
gsub x y str@(s:ss)
  | isPrefixOf x str = y ++ gsub x y (drop (length x) str)
  | otherwise = s:gsub x y ss


 {-
 参考
    [Haskell Error lexical error in string/character literal at character ' \r'](https://stackoverflow.com/questions/27329771/haskell-error-lexical-error-in-string-character-literal-at-character-r)

    [Haskellで日本語を正規表現で置換](https://qiita.com/acro5piano/items/96d30d035bea527b1d24)

    [haskellで部分列置換](https://bsq77.hatenadiary.org/entry/20130224/1361672367)
-}

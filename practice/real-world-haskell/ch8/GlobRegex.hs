module GlobRegex (globToRegex, matchesGlob) where

import Text.Regex.Posix
    ((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex cs = (('^' : ) . (++ "$")) `fmap` globToRegex' cs

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat =
    case globToRegex pat of
        Right pat' -> name =~ pat'
        _          -> False

globToRegex' :: String -> Either GlobError String
globToRegex' cs =
    case cs of
        ""                   -> Right ""
        ('*' : cs)           -> (".*" ++) `fmap` globToRegex' cs
        ('?' : cs)           -> ('.' :) `fmap` globToRegex' cs
        ('[' : '!' : c : cs) -> (("[^" ++) . (c : )) `fmap` charClass cs
        ('[' : c : cs)       -> (('[' :) . (c :)) `fmap` charClass cs
        ['[']                -> Left "unterminated character class"
        (c : cs)             -> (escape c ++) `fmap` globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
         where regexChars = "\\+()^$.{}]|"

charClass :: String -> Either GlobError String
charClass cs =
    case cs of
        (']' : cs) -> (']' : ) `fmap` globToRegex' cs
        (c : cs)   -> (c : ) `fmap` charClass cs
        []         -> Left "unterminated character class"

module GlobRegex (globToRegex, matchesGlob) where

import Text.Regex.Posix
    ((=~))

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat

globToRegex' :: String -> String
globToRegex' cs =
    case cs of
        ""                   -> ""
        ('*' : cs)           -> ".*" ++ globToRegex' cs
        ('?' : cs)           -> '.' : globToRegex' cs
        ('[' : '!' : c : cs) -> "[^" ++ c : charClass cs
        ('[' : c : cs)       -> '[' : c : charClass cs
        ['[']                -> error "unterminated character class"
        (c : cs)             -> escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
         where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass cs =
    case cs of
        (']' : cs) -> ']' : globToRegex' cs
        (c : cs)   -> c : charClass cs
        []         -> error "unterminated character class"

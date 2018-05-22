module Try.CSV (parseCSV) where

import Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile = do
    h <- header <* eol
    lines <- endBy1 (line $ length h) eol
    return $ h : lines

header :: GenParser Char st [String]
header = sepBy cell (char ',')

line :: Int -> GenParser Char st [String]
line n = sepByN n cell (char ',')

sepByN :: Int -> GenParser Char st a ->
          GenParser Char st sep -> GenParser Char st [a]
sepByN 0 _ _ = return []
sepByN 1 a _ = do
    res <- a
    return [res]
sepByN n a sep = do
    h <- a
    sep
    t <- sepByN (n - 1) a sep
    return $ h : t


cell :: GenParser Char st String
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell :: GenParser Char st String
quotedCell = do
    char '"'
    content <- many quotedChar
    char '"' <?> "quote at end of cell"
    return content

quotedChar :: GenParser Char st Char
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")

eol :: GenParser Char st String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> try (string "\n")
    <|> try (string "\r")
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile ""

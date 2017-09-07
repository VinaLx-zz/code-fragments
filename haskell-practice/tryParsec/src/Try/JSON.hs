module Try.JSON where

import qualified Data.Map                      as M
import           Debug.Trace
import           Text.ParserCombinators.Parsec

data Json = JNumber Double
          | JString String
          | JNull
          | JArray [Json]
          | JObject (M.Map String Json)
          | JBool Bool
          deriving (Show)

json :: GenParser Char st Json
json = symbol json'

json' :: GenParser Char st Json
json' =  jbool
     <|> jstring
     <|> jnumber
     <|> jarray
     <|> jobject
     <|> jnull

jstring :: GenParser Char st Json
jstring = JString <$> surroundBy (char '"') (many jchar)
    where jchar :: GenParser Char st Char
          jchar =  noneOf "\"\\" <|> (char '\\'
                >> ('\"' <$ try (char '"')
                <|> '\r' <$ try (char 'r')
                <|> '\\' <$ try (char '\\')
                <|> '\n' <$ try (char 'n')
                <|> '\t' <$ try (char 't')
                <?> "valid escape character"))

jarray :: GenParser Char st Json
jarray = JArray <$> between (char '[') (char ']') jsons
    where jsons = try (json `sepBy` char ',') <|> [] <$ spaces

jobject :: GenParser Char st Json
jobject = JObject . M.fromList <$> between (char '{') (char '}') pairs
    where pairs = try (pair `sepBy` char ',') <|> [] <$ spaces
          pair = do
            JString s <- symbol jstring
            char ':'
            j <- json
            return (s, j)

surroundBy :: GenParser Char st a -> GenParser Char st b -> GenParser Char st b
surroundBy s = between s s

symbol :: GenParser Char st a -> GenParser Char st a
symbol = surroundBy spaces

symbolS :: String -> GenParser Char st String
symbolS = surroundBy spaces . string

symbolC :: Char -> GenParser Char st Char
symbolC = surroundBy spaces . char

jnumber :: GenParser Char st Json
jnumber = JNumber <$> number
  where number :: GenParser Char st Double
        number = do
            f <- floating
            e <- exponent
            power <- if e then decimal else return 0
            return $ f * 10 ** fromIntegral power
        exponent :: GenParser Char st Bool
        exponent = True <$ oneOf "Ee" <|> pure False
        decimal :: GenParser Char st Int
        decimal = do
            neg <- negative
            (* (-1) ^ neg) . read <$> many1 digit
        negative :: GenParser Char st Int
        negative = 1 <$ char '-' <|> 0 <$ char '+' <|> pure 0
        floating :: GenParser Char st Double
        floating = do
            i <- decimal
            d <- (char '.' >> many1 digit) <|> return "0"
            let op = if i >= 0 then (+) else (-)
            return $ fromIntegral i `op` (read d / fromIntegral (10 ^ length d))

jnull :: GenParser Char st Json
jnull = string "null" >> return JNull

jbool :: GenParser Char st Json
jbool = JBool <$> (True <$ string "true" <|> False <$ string "false")


parseJson :: String -> Either ParseError Json
parseJson = parse json ""

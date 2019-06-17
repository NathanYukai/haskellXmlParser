{-# LANGUAGE FlexibleContexts#-}
module XmlParser where

import qualified Data.Text as T

import Text.Parsec
import Debug.Trace
import Data.Functor.Identity
import Types

-- utils
test = testParser elementTreeParser exampleOne
exampleOne = "<root>\n  <child1>  \n leaf1   \n    </child1>\n<child2><fjdksl />  </child2>\n</root>"

testParser p = parse p ""

println msg = trace (show msg) $ return ()

seeNext :: String -> Int -> ParsecT String u Identity ()
seeNext msg n = do
  s <- getParserState
  let out = take n (stateInput s)
  println (msg ++ " -- --:" ++ out)

skipPossibleSpaces :: Parsec String st ()
skipPossibleSpaces = skipMany (char ' ')

-- tag values parser : e.g. <Tag TagValueKey="Tagvalue value"/>
tagValueKeyParser :: Parsec String st String
tagValueKeyParser = many1 $ noneOf "= "

tagValueValueParser :: Parsec String st String
tagValueValueParser = between (char '"') (char '"') (many $ noneOf "\"")

singleTagValueParser :: Parsec String st (String,String)
singleTagValueParser = do
  key <- tagValueKeyParser
  skipPossibleSpaces
  _ <- char '='
  skipPossibleSpaces
  val <- tagValueValueParser
  return (key, val)

tagValuesParser :: Parsec String st TagValues
tagValuesParser =
  manyTill
    (do v <- singleTagValueParser
        skipPossibleSpaces
        return v)
    (lookAhead $ choice [char '>', ((char '/') >> char '>')])

simpleTagParser :: Parsec String st Tag
simpleTagParser =
  between
    (char '<')
    (string "/>")
    (do tagName <- many1 $ noneOf " /"
        skipPossibleSpaces
        values <- tagValuesParser
        return $ SimpleTag tagName values)

openTagParser :: Parsec String st Tag
openTagParser =
  between
    (char '<')
    (char '>')
    (do skipPossibleSpaces
        tagName <- many1 $ noneOf " >/"
        skipPossibleSpaces
        values <- tagValuesParser
        return $ OpenTag tagName values)

closeTagParser :: Parsec String st Tag
closeTagParser =
  do
    _ <- string "</"
    spaces
    _ <- many1 $ noneOf "<>"
    _ <- char '>'
    return $ CloseTag ""

-- element parser
leafParser :: Parsec String st Element
leafParser = do
  val <- manyTill anyChar (lookAhead $ string "</")
  let valTrimed = T.strip $ T.pack val
  return $ Leaf $ T.unpack valTrimed

elementTreeParser :: Parsec String st Element
elementTreeParser
  -- seeNext "tree parser enter" 100
 = do
  _ <- char '<'
  spaces
  elementName <- many1 (letter <|> digit)
  spaces
  attr <- tagValuesParser
  spaces
  end <- try (string "/>" <|> string ">")
  if (length end == 2)
    then spaces *> (return $ Node elementName attr [])
    else do
      (try
         (do spaces
             _ <- closeTagParser
             return $ Node elementName attr [])) <|>
        (do elemBody <- many elementBodyParser
            _ <- closeTagParser
            spaces
            return $ Node elementName attr elemBody)

elementBodyParser :: Parsec String st Element
elementBodyParser = spaces *> try (elementTreeParser <|> leafParser)


module Main where

import Data.Text
import XmlParser
import XmlUtils

main :: IO ()
main = do
  putStrLn "hello world"

xmlFileParser :: String -> IO ()
xmlFileParser filePath = do
  file_content <- readFile filePath
  let xml = testParser elementTreeParser file_content
  case xml of
    (Left err) -> putStrLn $ show err
    (Right e) -> writeFile "prettyXml.xml" (unpack $ prettyPrintXml e 0)

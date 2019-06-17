{-# LANGUAGE OverloadedStrings #-}
module XmlUtils where

import Types
import qualified Data.Text as T

changeNodeName :: Element -> String -> String -> Element
changeNodeName (Leaf l) _ _ = Leaf l
changeNodeName (Node name tv children) matchName newName =
  case name == matchName of
    True -> Node newName tv newChildren
    False -> Node name tv newChildren
  where
    newChildren = fmap (\c -> changeNodeName c matchName newName) children

prettyPrintTagValues :: TagValues -> T.Text
prettyPrintTagValues [] = ""
prettyPrintTagValues ((tn,tv):ts) = T.concat [T.pack tn , "=" , "\"", T.pack tv , "\" "]

getIndentation :: Int -> T.Text
getIndentation n = T.concat $ replicate n "  "

printNameAndValues :: String -> TagValues -> T.Text
printNameAndValues name tagValues =
  T.concat ["<" , T.pack name, " ", (prettyPrintTagValues tagValues), ">"]

prettyPrintXml :: Element -> Int -> T.Text
prettyPrintXml (Node nodeName tagValues []) level =
  T.concat [getIndentation level, "<", T.pack nodeName, " ", (prettyPrintTagValues tagValues), "/>\n"]
prettyPrintXml (Node nodeName tagValues [Leaf l]) level =
  T.concat [getIndentation level ,
  (printNameAndValues nodeName tagValues) , T.pack l , "</" , T.pack nodeName , " >\n"]
prettyPrintXml (Node nodeName tagValues children) level =
  T.concat
    [ getIndentation level
    , (printNameAndValues nodeName tagValues)
    , "\n"
    , T.concat $ fmap (\c -> prettyPrintXml c (level + 1)) children
    , getIndentation level
    , "</"
    , T.pack nodeName
    , " >\n"
    ]

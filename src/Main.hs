
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Text.HTML.TagSoup (Tag (..), isTagCloseName, parseTags)
import Data.List (intercalate)
import Data.Foldable (for_)
import Options.Generic
import Data.Maybe (fromMaybe)

data Options = Options {
    indent :: Maybe Int
  , inputFile :: FilePath
  , outputFile :: Maybe FilePath
} deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = do
  opts <- getRecord "miso-from-html"
  toMisoFile opts

toMisoFile :: Options -> IO ()
toMisoFile Options{..} = do
  tags <- cleanup . parseTags <$> readFile inputFile
  let miso = tagToMiso $ cleanup tags
      i = fromMaybe 2 indent
  case outputFile of
    Nothing -> out putStrLn i miso
    Just f -> out (\input -> appendFile f (input ++ "\n\n")) i miso
  where
    out outMethod i miso =
      for_ (zip miso [1..length miso]) $ \(m, j) ->
        outMethod $ "fn" ++ (show j) ++ " = " ++ showMisoIndent i m

cleanup :: [Tag String] -> [Tag String]
cleanup [] = []
cleanup (TagText s:xs)
  | null (strip s) = cleanup xs
  | otherwise = TagText (strip s) : cleanup xs
    where
      strip = reverse . dropThings . reverse . dropThings
      dropThings = dropWhile (\x -> x == '\t' || x == '\n' || x == ' ')
cleanup (x:xs) = x : cleanup xs

type Indent = Int

data MisoAttribute str = MisoAttribute str str deriving (Show)

data Miso str = MisoElement str [MisoAttribute str] [Miso str]
              | MisoText str
              deriving (Show)

tagToMiso :: [Tag String] -> [Miso String]
tagToMiso [] = []
tagToMiso (TagOpen name attrs : xs) =
  let (innerTags, sameLevelTags) = break (isTagCloseName name) xs
  in MisoElement name (map toMisoAttribute attrs) (tagToMiso innerTags) : (tagToMiso sameLevelTags)
  where
    toMisoAttribute (x, y) = MisoAttribute x y
tagToMiso (TagText txt : xs) = MisoText txt : tagToMiso xs
tagToMiso (_ : xs) = tagToMiso xs

showMiso :: Miso String -> String
showMiso (MisoText txt) = "text \"" ++ txt ++ "\""
showMiso (MisoElement name attrs elems) =
  name ++ "_ " ++ showAttrs attrs ++ " [ " ++ intercalate ", " (map showMiso elems) ++ " ]"
  where
    showAttrs [] = "[]"
    showAttrs [a] = "[ " ++ showAttr a ++ " ]"
    showAttrs as = "[ " ++ intercalate ", " (map showAttr as) ++ " ]"
    showAttr (MisoAttribute n v) = n ++ "_ \"" ++ v ++ "\""

showMisoIndent :: Indent -> Miso String -> String
showMisoIndent _ (MisoText txt) = "text \"" ++ txt ++ "\""
showMisoIndent indent (MisoElement name attrs elems) =
  name ++ "_ " ++ line ++ showAttrs attrs ++ line ++
  case elems of
    [] -> ""
    m@(MisoText _) : rest -> "[ " ++ showMisoIndent indent m ++ " ]" ++ listIndent (map (showMisoIndent (indent + indent)) rest)
    _ -> "[ " ++ listIndent (map (showMisoIndent (indent + indent)) elems) ++ line ++ "]"
  where
    showAttrs [] = "[]"
    showAttrs [a] = "[ " ++ showAttr a ++ " ]"
    showAttrs as = "[ " ++ listIndent (map showAttr as) ++ " ]"
    showAttr (MisoAttribute n v) = n ++ "_ \"" ++ v ++ "\""
    line = "\n" ++ replicate indent ' '
    listIndent as = intercalate (line ++ ", ") as

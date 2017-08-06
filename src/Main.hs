
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

-- | Command line options
data Options = Options {
    indent :: Maybe Int          -- | Level of indentation
  , inputFile :: FilePath        -- | HTML file to be parsed
  , outputFile :: Maybe FilePath -- | .hs output file
} deriving (Generic, Show)

instance ParseRecord Options

--------------------------------------------------------------------------------


main :: IO ()
main = do
  opts <- getRecord "miso-from-html"
  toMisoFile opts


--------------------------------------------------------------------------------


toMisoFile :: Options -> IO ()
toMisoFile Options{..} = do
  -- | Get the tags and cleanup up noise
  tags <- cleanup . parseTags <$> readFile inputFile

  -- | Generate intermediate data structure `Miso`
  -- | and get level of indentation
  let miso = tagToMiso $ cleanup tags
      i = fromMaybe 2 indent

  -- | If output file was not given then print to console
  -- | otherwise write to the file
  case outputFile of
    Nothing -> out putStrLn i miso
    Just f -> out (\input -> appendFile f (input ++ "\n\n")) i miso
  where
    out outMethod i miso =
      let show_ = if i == 0 then showMiso else showMisoIndent i
      in for_ (zip miso [1..length miso]) $ \(m, j) ->
          outMethod $ "fn" ++ (show j) ++ " = " ++ show_ m


--------------------------------------------------------------------------------


cleanup :: [Tag String] -> [Tag String]
cleanup [] = []
cleanup (TagText s:xs)
  | null (strip s) = cleanup xs
  | otherwise = TagText (strip s) : cleanup xs
    where
      strip = reverse . dropThings . reverse . dropThings
      dropThings = dropWhile (\x -> x == '\t' || x == '\n' || x == ' ')
cleanup (x:xs) = x : cleanup xs


--------------------------------------------------------------------------------


type Indent = Int


-- | MisoAttribute is a synonym for the name, value pair of a HTML Attribute
data MisoAttribute str = MisoAttribute str str

-- | The Miso structure converts the nest HTML into a more manageable structure
data Miso str = MisoElement str [MisoAttribute str] [Miso str]
              | MisoText str


--------------------------------------------------------------------------------

-- | Convert a list of HTML tags to a list of Miso
-- | Each element of the output should be a single nested HTML structure
-- | `<head></head> <body></body> == fn1 = head_; fn2 = body_`
tagToMiso :: [Tag String] -> [Miso String]
tagToMiso [] = []
tagToMiso (TagOpen name attrs : xs) =
  -- | Search for the corresponding closing tag
  -- | If the nested tags have the same name this still works out
  let (innerTags, sameLevelTags) = break (isTagCloseName name) xs

  -- | Create a MisoElement and move on
  in MisoElement name (map toMisoAttribute attrs) (tagToMiso innerTags) : (tagToMiso sameLevelTags)
  where
    toMisoAttribute (x, y) = MisoAttribute x y
tagToMiso (TagText txt : xs) = MisoText txt : tagToMiso xs
tagToMiso (_ : xs) = tagToMiso xs  -- | Ignore everything else


--------------------------------------------------------------------------------


-- | Flat version of show
showMiso :: Miso String -> String
showMiso (MisoText txt) = "text \"" ++ txt ++ "\""
showMiso (MisoElement name attrs elems) =
  name ++ "_ " ++ showAttrs attrs ++ " [ " ++ intercalate ", " (map showMiso elems) ++ " ]"
  where
    showAttrs [] = "[]"
    showAttrs [a] = "[ " ++ showAttr a ++ " ]"
    showAttrs as = "[ " ++ intercalate ", " (map showAttr as) ++ " ]"
    showAttr (MisoAttribute n v) = n ++ "_ \"" ++ v ++ "\""


--------------------------------------------------------------------------------


-- | Indented version of show
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


--------------------------------------------------------------------------------

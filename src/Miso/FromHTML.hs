
module Miso.FromHTML
    ( toMiso
    , toMisoIndent
    ) where

import Data.List (intercalate)
import Data.Tree.NTree.TypeDefs (NTree (..))
import Text.XML.HXT.DOM.TypeDefs (XNode (..), QName)
import Text.XML.HXT.DOM.QualifiedName (localPart)
import Text.XML.HXT.Core ( withParseHTML
                         , withWarnings
                         , withRemoveWS
                         , withIgnoreNoneXmlContents
                         , getChildren
                         , readString
                         , runX
                         , yes, no
                         , (>>>))

import Debug.Trace

toMiso :: String -> IO [String]
toMiso input = do
  let doc = readString [withParseHTML yes, withRemoveWS yes, withWarnings yes] input
  children <- runX $ doc >>> getChildren
  pure $ map showMiso children

toMisoIndent :: Int -> String -> IO [String]
toMisoIndent indent input = do
  let doc = readString [withParseHTML yes, withRemoveWS yes, withWarnings yes, withIgnoreNoneXmlContents yes] input
  children <- runX $ doc >>> getChildren
  pure $ map (showMisoIndent indent) children

toFunctionName :: QName -> String
toFunctionName name = localPart name ++ "_ "

showMiso :: NTree XNode -> String
showMiso (NTree (XTag name attrs) trees) =
  toFunctionName name ++ showAttrs attrs ++ " [ " ++ intercalate ", " (map showMiso trees) ++ " ]"
  where
    showAttrs [] = "[]"
    showAttrs [a] = "[ " ++ showMiso a ++ " ]"
    showAttrs as = "[ " ++ intercalate ", " (map showMiso as) ++ " ]"
showMiso (NTree (XText text) _) = "text " ++ show text
showMiso (NTree (XAttr name) attr) =
  toFunctionName name ++ case attr of
    [NTree (XText text) []] -> show text
    _                       -> ""
showMiso (NTree _ trees) = " [ " ++ intercalate ", " (map showMiso trees) ++ " ]"

showMisoIndent :: Int -> NTree XNode -> String
showMisoIndent indent (NTree (XTag name attrs) trees) =
  toFunctionName name ++ line ++ showAttrs attrs ++ line ++
  case trees of
    [] -> "[]"
    (t@(NTree (XText _) []) : rest) -> "[ " ++ showMisoIndent indent t ++ " ]" ++ listIndent (map (showMisoIndent (indent + 2)) rest)
    _ -> "[ " ++ listIndent (map (showMisoIndent (indent + 2)) trees) ++ line ++ "]"
  where
    showAttrs [] = "[]"
    showAttrs [a] = "[ " ++ showMisoIndent indent a ++ " ]"
    showAttrs as = "[ " ++ listIndent (map (showMisoIndent indent) as) ++ " ]"
    line = "\n" ++ replicate indent ' '
    listIndent as = intercalate (line ++ ", ") as
showMisoIndent _ (NTree (XText text) _) = "text " ++ show text
showMisoIndent _ (NTree (XAttr name) attr) =
  toFunctionName name ++ case attr of
    [NTree (XText text) []] -> show text
    _                       -> ""
showMisoIndent indent (NTree _ trees) = " [ " ++ intercalate ", " (map (showMisoIndent indent) trees) ++ " ]"

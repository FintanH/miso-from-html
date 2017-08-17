
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Foldable (for_)
import Options.Generic
import Data.Maybe (fromMaybe)
import Miso.FromHTML

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
  input <- readFile inputFile

  -- | get level of indentation defaulting to 2
  let i = fromMaybe 2 indent

  -- | If output file was not given then print to console
  -- | otherwise write to the file
  case outputFile of
    Nothing -> out putStrLn i input
    Just f -> out (\input' -> appendFile f (input' ++ "\n\n")) i input
  where
    out outMethod i input = do
      let show_ = if i == 0 then toMiso else toMisoIndent i
      miso <- show_ input
      for_ (zip miso [1..length miso]) $ \(m, j) ->
        outMethod $ "fn" ++ (show j) ++ " = " ++ m


--------------------------------------------------------------------------------

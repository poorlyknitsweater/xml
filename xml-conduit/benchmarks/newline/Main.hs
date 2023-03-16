module Main (main) where

import Conduit (runConduitRes, (.|))
import Control.DeepSeq (force)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (evaluate)
import Criterion.Main (defaultMain, bench, whnfIO)
import Data.Conduit.Binary (sourceFile)

import qualified Data.Conduit.Combinators as CC

import Text.XML.Stream.Parse (parseBytes, def)

main :: IO ()
main = do
  defaultMain
    [ bench "with newline normalization" $ whnfIO (parse "./big.xml")
    ]

  where
    parse :: FilePath -> IO ()
    parse path = runConduitRes $
      sourceFile path
      .| parseBytes def
      .| CC.mapM (liftIO . evaluate . force)
      .| CC.sinkNull

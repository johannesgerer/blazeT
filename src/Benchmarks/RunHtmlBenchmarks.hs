-- | This is a module which runs the 'HtmlBenchmarks' module using the different
-- renderers available.
--
module Benchmarks.RunHtmlBenchmarks where

import Criterion.Main
import qualified Data.Text.Lazy as LT
import Data.List
import qualified Data.ByteString.Lazy as LB

import qualified Text.Blaze.Renderer.Utf8 as Utf8
import qualified Text.Blaze.Renderer.String as String
import qualified Text.Blaze.Renderer.Text as Text

import Benchmarks.HtmlBenchmarks (HtmlBenchmark (..), benchmarks)
import qualified Benchmarks.BlazeTBenchmarks as BT (HtmlBenchmark (..), benchmarks)
import qualified Text.BlazeT.Renderer.Utf8 as TUtf8
import qualified Text.BlazeT.Renderer.String as TString
import qualified Text.BlazeT.Renderer.Text as TText

-- | Function to run the benchmarks using criterion
--
main :: IO ()
main = defaultMain $ zipWith g benchmarks BT.benchmarks
  where
    g x y = bgroup (hName x) $ benchHtml x ++ benchHtml2 y
    benchHtml  (HtmlBenchmark _ f x _) = 
        [ bench "Utf8"   $ nf (LB.length .  Utf8.renderMarkup . f) x
        , bench "String" $ nf (String.renderMarkup . f) x
        , bench "Text"   $ nf (LT.length . Text.renderMarkup . f) x
        ]
    benchHtml2 (BT.HtmlBenchmark _ f x _) =
        [ bench "BlazeT.Utf8"   $ nf (LB.length .  TUtf8.renderMarkup . f) x
        , bench "BlazeT.String" $ nf (TString.renderMarkup . f) x
        , bench "BlazeT.Text"   $ nf (LT.length . TText.renderMarkup . f) x
        ]

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Hasel (World (..), allWorlds, haselIt)
import Hasel.SVG (generateAllIcons)
import Hasel.Typst (generateTypstDocument)

main :: IO ()
main = do
  putStrLn "🐿️ HASEL — Haskell + Emoji Language"
  putStrLn "═══════════════════════════════════════"
  putStrLn ""

  -- Display all worlds
  mapM_ printWorld allWorlds
  putStrLn ""

  -- Generate SVG icons
  putStrLn "Generating SVG icons..."
  mapM_ writeIcon generateAllIcons
  putStrLn $ "  ✓ " ++ show (length generateAllIcons) ++ " icons generated"
  putStrLn ""

  -- Generate Typst document
  putStrLn "Generating Typst document..."
  TIO.writeFile "docs/hasel.typ" generateTypstDocument
  putStrLn "  ✓ docs/hasel.typ written"
  putStrLn ""

  -- Demo: HASEL IT
  putStrLn "🐿️ HASEL IT: \"types web browser\""
  let results = haselIt "types web browser"
  mapM_ (\w -> putStrLn $ "  " ++ T.unpack (worldEmoji w) ++ " " ++ T.unpack (worldName w)) results
  putStrLn ""

  putStrLn "🌰 Done. HASEL IT!"

printWorld :: World -> IO ()
printWorld w = do
  putStrLn $ T.unpack (worldEmoji w) ++ " " ++ T.unpack (worldName w)
  putStrLn $ "  " ++ T.unpack (worldTagline w)
  putStrLn $ "  Features: " ++ show (length (worldFeatures w))
  putStrLn $ "  Insights: " ++ show (length (worldInsights w))
  putStrLn ""

writeIcon :: (Text, Text) -> IO ()
writeIcon (path, content) = do
  TIO.writeFile (T.unpack path) content
  putStrLn $ "  → " ++ T.unpack path

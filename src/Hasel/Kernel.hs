{-# LANGUAGE OverloadedStrings #-}

-- ✳ HASEL KERNEL — Schicht 3 (ASCII/Haskell)
-- 🐿️ Kern-Operationen: nuss, wald, filterFuchs, filterSchlange
-- λ Kernel → pipeline :: a -> IO Frozen
-- Aus: ✳ Squirrel OS Architecture.txt, Zeilen 95-117

module Hasel.Kernel where

import Hasel.Types

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, doesFileExist)

-- | 🌰→🪺  jeder Wert ist Monade ist Nuss
nuss :: a -> Maybe a
nuss = Just

-- | 🪹→¬🌰  Abwesenheit ist auch Typ
leer :: Maybe a
leer = Nothing

-- | 🌳 Wald prüfen: existiert der KOBEL?
wald :: FilePath -> IO (Maybe FilePath)
wald path = do
  exists <- doesDirectoryExist path
  if exists then return (nuss path) else return leer

-- | ¬🦊 Filter: keine versteckten System-Dateien (.git etc.)
filterFuchs :: [KobelNode] -> [KobelNode]
filterFuchs = filter (not . isFuchs)
  where
    isFuchs node = any (`elem` fuchsPaths) [knRelPath node]
    fuchsPaths =
      [ ".git", ".gitignore", ".gitattributes"
      , "node_modules", ".stack-work", "dist-newstyle"
      ]

-- | ¬🦊 Für Pfad-Prüfung: ist ein Pfad-Segment ein Fuchs?
isFuchsSegment :: FilePath -> Bool
isFuchsSegment seg = seg `elem`
  [ ".git", "node_modules", ".stack-work", "dist-newstyle"
  , ".obsidian", "__pycache__", ".claude"
  ]

-- | ¬🐍 Filter: keine gefährlichen Dateien
filterSchlange :: [KobelNode] -> [KobelNode]
filterSchlange = filter (not . isSchlange)
  where
    isSchlange node = any (`T.isSuffixOf` T.pack (knPath node)) schlangeExts
    schlangeExts = [".tmp", ".lock", ".bak"]

-- | 📊 Statistik ausgeben
printStats :: [KobelNode] -> IO ()
printStats nodes = do
  let dirs  = length $ filter knIsDir nodes
      files = length $ filter (not . knIsDir) nodes
      total = sum $ map knSize nodes
      mb    = fromIntegral total / (1024 * 1024) :: Double
  TIO.putStrLn $ T.concat
    [ "🐿️ KOBEL-Scan: "
    , T.pack (show dirs), " 📁 Ordner, "
    , T.pack (show files), " 📄 Dateien, "
    , T.pack (show (round mb :: Int)), " MB"
    ]

-- | ❄️ Freeze-Ergebnis als Text
freezeToText :: FreezeResult -> Text
freezeToText fr = T.unlines
  [ "❄️ GEFRIERTROCKNUNG — Ergebnis"
  , "═══════════════════════════════════"
  , T.concat ["🚪 Verschoben:  ", T.pack (show (frMoved fr))]
  , T.concat ["📁 Erstellt:    ", T.pack (show (frCreated fr))]
  , T.concat ["📝 Generiert:   ", T.pack (show (frGenerated fr))]
  , T.concat ["⏭️  Übersprungen: ", T.pack (show (frSkipped fr))]
  , "═══════════════════════════════════"
  , frManifest fr
  ]

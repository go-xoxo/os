{-# LANGUAGE OverloadedStrings #-}

-- ✳ HASEL README — Schicht 3 (ASCII/Haskell)
-- 📝 Generiert ein README.md pro KOBEL-Ordner
-- λ Readme → generateReadme :: KobelCategory -> [KobelNode] -> Text

module Hasel.Readme where

import Hasel.Types

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T

-- | 📝 README für eine Kategorie generieren
generateReadme :: KobelCategory -> [KobelNode] -> T.Text
generateReadme cat nodes = T.unlines $
  [ T.concat ["# ", categoryEmoji cat, " ", T.pack (show cat)]
  , ""
  , categoryDescription cat
  , ""
  , T.concat ["**", T.pack (show fileCount), "** Dateien · **"
             , T.pack (show dirCount), "** Ordner · **"
             , T.pack (show (totalMB nodes)), " MB**"]
  , ""
  , "## Inhalt"
  , ""
  ] ++ fileList
  where
    fileCount = length $ filter (not . knIsDir) nodes
    dirCount  = length $ filter knIsDir nodes
    fileList  = map formatNode (sortBy (comparing knRelPath) (take 50 nodes))

-- | Einen Knoten als Markdown-Zeile formatieren
formatNode :: KobelNode -> T.Text
formatNode node
  | knIsDir node = T.concat ["- 📁 `", T.pack (knRelPath node), "/`"]
  | otherwise    = T.concat
      [ "- ", fileIcon (knRelPath node)
      , " `", T.pack (knRelPath node), "`"
      , sizeSuffix (knSize node)
      ]

-- | Datei-Icon basierend auf Extension
fileIcon :: FilePath -> T.Text
fileIcon path
  | ".hs"    `isSuffix` path = "🌰"
  | ".hasel" `isSuffix` path = "🐿️"
  | ".md"    `isSuffix` path = "📄"
  | ".txt"   `isSuffix` path = "📝"
  | ".py"    `isSuffix` path = "🐍"
  | ".ps1"   `isSuffix` path = "💪"
  | ".nu"    `isSuffix` path = "⚡"
  | ".zip"   `isSuffix` path = "📦"
  | ".exe"   `isSuffix` path = "⚙️"
  | ".cabal" `isSuffix` path = "📋"
  | otherwise                = "📎"
  where isSuffix s p = T.isSuffixOf (T.pack s) (T.pack p)

-- | Dateigröße als Suffix
sizeSuffix :: Integer -> T.Text
sizeSuffix bytes
  | bytes < 1024        = T.concat [" (", T.pack (show bytes), " B)"]
  | bytes < 1048576     = T.concat [" (", T.pack (show (bytes `div` 1024)), " KB)"]
  | otherwise           = T.concat [" (", T.pack (show (bytes `div` 1048576)), " MB)"]

-- | Gesamtgröße in MB
totalMB :: [KobelNode] -> Int
totalMB nodes = fromIntegral (sum (map knSize nodes)) `div` (1024 * 1024)

-- | Beschreibung für jede Kategorie
categoryDescription :: KobelCategory -> T.Text
categoryDescription Verzeichnisbaum = "🌲🌳🌴🏡🎋🎄🪵 Quellcode und Haskell-Module des Squirrel OS."
categoryDescription Datenbank       = "💾🔒🔏🔐 Archivierte Daten, haselifizierte Dokumente und ZIP-Archive."
categoryDescription UserDoku        = "🏄🏿\x200d♀️🐬🐚🌴🌴🦀🏖️🏝️⛱️🩴 Benutzer-Dokumentation."
categoryDescription WeltDoku        = "🗺️👽🧑🏿\x200d🚀👨🏿\x200d🚀👩🏿\x200d🚀🌍🌎🌏🛰️🚀☄️ Architektur und OS-Spezifikation."
categoryDescription ModelDoku       = "🐿️ HASEL-Selbstdokumentation — Doku in HASEL für HASEL SELB."
categoryDescription ChatExport      = "📝 Exportierte Chat-Sitzungen und Konversationsarchive."
categoryDescription Projekt         = "🔨 Projekte, Tools, Skripte und Utilities."
categoryDescription Vendored        = "📦 Externe Abhängigkeiten und Bibliotheken."
categoryDescription Artefakt        = "⚙️ Build-Artefakte (.exe, .hi, .o Dateien)."
categoryDescription Unbekannt       = "❓ Nicht klassifizierte Dateien."

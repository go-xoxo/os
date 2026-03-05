{-# LANGUAGE OverloadedStrings #-}

-- ✳ HASEL KOBEL — Schicht 3 (ASCII/Haskell)
-- 🏡 Scanner und Kategorisierer für den KOBEL-Verzeichnisbaum
-- λ Kobel → scanKobel >=> categorizeAll

module Hasel.Kobel where

import Hasel.Types
import Hasel.Kernel (isFuchsSegment)

import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf, intercalate)
import qualified Data.Text as T
import System.Directory
    ( listDirectory
    , doesDirectoryExist
    , getFileSize
    , makeAbsolute
    )
import System.FilePath ((</>), takeExtension, takeFileName, splitDirectories)

-- | 🌰→🐿️ haselifizieren: KOBEL scannen
scanKobel :: FilePath -> IO [KobelNode]
scanKobel root = do
  absRoot <- makeAbsolute root
  go absRoot absRoot
  where
    go base dir = do
      entries <- listDirectory dir
      concat <$> mapM (processEntry base dir) entries

    processEntry base parent name = do
      let fullPath = parent </> name
      if isFuchsSegment name
        then return []  -- ¬🦊 Fuchs gefiltert
        else do
          isDir <- doesDirectoryExist fullPath
          if isDir
            then do
              let node = KobelNode
                    { knPath     = fullPath
                    , knRelPath  = makeRel base fullPath
                    , knIsDir    = True
                    , knSize     = 0
                    , knCategory = Unbekannt
                    }
              children <- go base fullPath
              return (node : children)
            else do
              size <- getFileSize fullPath
              let node = KobelNode
                    { knPath     = fullPath
                    , knRelPath  = makeRel base fullPath
                    , knIsDir    = False
                    , knSize     = size
                    , knCategory = Unbekannt
                    }
              return [node]

    makeRel base path =
      let baseSegs = splitDirectories base
          pathSegs = splitDirectories path
          rel = drop (length baseSegs) pathSegs
      in intercalate "/" rel

-- | 🐿️→⚡ nuifizieren: alle Knoten kategorisieren
categorizeAll :: [KobelNode] -> [KobelNode]
categorizeAll = map (\n -> n { knCategory = categorize n })

-- | TODO(human): Kategorisierungslogik
-- Diese Funktion bestimmt, in welche KOBEL-Kategorie eine Datei gehört.
-- Sie wird auf JEDEN Knoten im Verzeichnisbaum angewandt.
categorize :: KobelNode -> KobelCategory
categorize node
  -- Verzeichnisse
  | knIsDir node = categorizeDir (knRelPath node)
  -- Dateien
  | otherwise    = categorizeFile (knRelPath node) ext
  where
    ext = map toLower $ takeExtension (knPath node)

-- | Ordner kategorisieren
categorizeDir :: FilePath -> KobelCategory
categorizeDir rel
  | "src"     `isPrefixOf` rel = Verzeichnisbaum
  | "app"     `isPrefixOf` rel = Verzeichnisbaum
  | "data"    `isPrefixOf` rel = Datenbank
  | "IHaskell" `isPrefixOf` rel = Vendored
  | "projekte" `isPrefixOf` rel = Projekt
  | "claude_code_sessions" `isPrefixOf` rel = ChatExport
  | "docs"    `isPrefixOf` rel = UserDoku
  | otherwise                  = Unbekannt

-- | Dateien kategorisieren
categorizeFile :: FilePath -> String -> KobelCategory
categorizeFile rel ext
  -- HASEL / Modeldoku
  | ".hasel" `isSuffixOf` rel        = ModelDoku
  -- Architektur-Dokumente = WeltDoku
  | "Architecture" `isInPath` rel     = WeltDoku
  | "eichhornchen-os" `isInPath` rel  = WeltDoku
  -- Chat-Exporte (große .txt Dateien im Root)
  | ext == ".txt" && '/' `notElem` rel = ChatExport
  -- Haskell source
  | ext == ".hs" && "data/" `isPrefixOf` rel = Datenbank
  | ext == ".hs"                       = Verzeichnisbaum
  -- Build-Artefakte
  | ext `elem` [".exe", ".hi", ".o"]  = Artefakt
  | ext == ".zip"                      = Datenbank
  -- Skripte = Projekt
  | ext `elem` [".py", ".ps1", ".nu"] = Projekt
  -- Markdown = Doku
  | ext == ".md"                       = UserDoku
  -- Alles andere
  | otherwise                          = Unbekannt
  where
    isInPath needle haystack = needle `isPrefixOf` takeFileName haystack
                            || ('/' : needle) `isPrefixOf` haystack
                            || needle `isSuffixOf` haystack

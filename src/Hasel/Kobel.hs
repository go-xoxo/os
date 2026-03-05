{-# LANGUAGE OverloadedStrings #-}

-- ✳ HASEL KOBEL — Schicht 3 (ASCII/Haskell)
-- 🏡 Scanner und Kategorisierer fuer den KOBEL-Verzeichnisbaum
-- λ Kobel → scanKobel >=> categorizeAll
--
-- ════════════════════════════════════════════════════════════════
-- ERKENNTNISSE AUS DEM ECHTEN KOBEL (Stand: 2026-03-05)
-- ════════════════════════════════════════════════════════════════
--
-- ROOT-STRUKTUR:
--   app/            → Verzeichnisbaum  (Main.hs Einstiegspunkt)
--   src/            → Verzeichnisbaum  (Hasel/*.hs, LLM/*.hs)
--   data/           → Datenbank        (emojifiles/, zips, .hasel)
--   project/        → ChatExport+Doku  (RIESIG: Notion, ChatGPT, Claude, Legal)
--   projekte/       → Projekt          (cabal, .nu, .py, Build-Artefakte)
--   claude_code_sessions/ → ChatExport (automatische Claude-Logs)
--   archive_unpack/ → Datenbank        (WhatsApp-Exporte)
--   IHaskell/       → Vendored         (externes Projekt)
--   txt/            → Datenbank        (Textdateien-Sammlung)
--   .vscode/        → Verzeichnisbaum  (IDE-Konfiguration)
--   .github/        → Verzeichnisbaum  (CI/CD, Copilot-Instruktionen)
--   .claude/        → Verzeichnisbaum  (Claude-Code-Settings)
--   .snapshots/     → Datenbank        (Konfig-Snapshots)
--   .stack-work/    → Artefakt         (Haskell-Build-Cache)
--   dist-newstyle/  → Artefakt         (Cabal-Build-Cache)
--   .venv/          → Vendored         (Python virtualenv)
--   .apy/           → Projekt          (Anki-Skript)
--   grok.com/       → ChatExport       (Grok-Exporte)
--   LINUX/          → WeltDoku         (Linux-Dokumentation)
--   obsidian_vault/ → UserDoku         (Notizen-Vault)
--   PerfLogs/       → Artefakt         (Windows Performance Logs)
--   webextension.org/ → Datenbank      (Browser-Extension-Downloads)
--
-- DATEI-MUSTER:
--   data/emojifiles/hs/*.hs → Datenbank  (Haselified Legal Docs, KEIN Code!)
--   data/✳ *.txt            → WeltDoku   (Architektur-Dokumente)
--   data/*.hasel            → ModelDoku  (HASEL-Manifeste)
--   project/ChatGPT-*.md    → ChatExport (ChatGPT-Exporte)
--   project/Claude-*.md     → ChatExport (Claude-Exporte)
--   project/chat-export*.md → ChatExport (Generische Chat-Exporte)
--   project/artefakt_*.md   → ChatExport (Session-Artefakte)
--   project/*_INDEX.md      → UserDoku   (Index-Dokumente)
--   project/*.html (UUID)   → ChatExport (Notion-Exporte)
--   project/*BESCHWERDE*.md → UserDoku   (Rechtsdokumente)
--   project/*KLAGESCHRIFT*  → UserDoku   (Rechtsdokumente)
--   project/*STRAFANZEIGE*  → UserDoku   (Rechtsdokumente)
--   project/*.json          → Datenbank  (Datenbank-Exporte, Chunks)
--   project/*.xlsx          → Datenbank  (Tabellen)
--   projekte/*.exe/.hi/.o   → Artefakt   (Haskell-Build-Artefakte)
--   projekte/*.nu           → Projekt    (Nushell-Skripte)
--   projekte/*.py           → Projekt    (Python-Skripte)
--
-- ════════════════════════════════════════════════════════════════

module Hasel.Kobel
  ( scanKobel
  , categorizeAll
  , categorize
  , categorizeDir
  , categorizeFile
  , shouldSkipDir
  ) where

import Hasel.Types
import Hasel.Kernel (isFuchsSegment)

import Control.Exception (try, SomeException)
import Data.Char (toLower, isHexDigit)
import Data.List (isPrefixOf, intercalate, isInfixOf)
import System.Directory
    ( listDirectory
    , doesDirectoryExist
    , getFileSize
    , makeAbsolute
    )
import System.FilePath ((</>), takeExtension, takeFileName, splitDirectories)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T


-- ═══════════════════════════════════════════════════════════
-- TEIL 1: KOBEL-SCANNER
-- 🌰→🐿️ haselifizieren :: FilePath -> IO [KobelNode]
-- Rekursiver Verzeichnisbaum-Walker mit Fuchs/Schlangen-Filter
-- ═══════════════════════════════════════════════════════════

-- | 🌰→🐿️ haselifizieren: KOBEL scannen
-- Laeuft rekursiv durch den gesamten Verzeichnisbaum,
-- filtert Fuchs-Segmente (node_modules, .git, etc.),
-- ueberspringt Build-Caches und Vendor-Dirs in der Tiefe
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
      -- 1. Fuchs-Filter (node_modules, .git, __pycache__, etc.)
      if isFuchsSegment name
        then return []
        else do
          -- Robuster Scanner: IO-Fehler fangen (Symlinks, Locks, etc.)
          result <- try (processEntryInner base fullPath) :: IO (Either SomeException [KobelNode])
          case result of
            Right nodes -> return nodes
            Left err -> do
              TIO.putStrLn $ T.concat
                [ "  ⚠️ Übersprungen (IO-Fehler): "
                , T.pack fullPath
                , " — ", T.pack (show err)
                ]
              return []  -- Fehlerhafte Eintraege graceful ueberspringen

    processEntryInner base fullPath = do
          isDir <- doesDirectoryExist fullPath
          if isDir
            then do
              let relP = makeRel base fullPath
              -- 2. Tiefe Verzeichnisse ueberspringen die nur Ballast sind
              --    (aber den Ordner selbst erfassen!)
              let node = KobelNode
                    { knPath     = fullPath
                    , knRelPath  = relP
                    , knIsDir    = True
                    , knSize     = 0
                    , knCategory = Unbekannt
                    }
              if shouldSkipDir relP
                then return [node]  -- Ordner registrieren, aber nicht hineinsteigen
                else do
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

-- | Verzeichnisse, in die wir NICHT rekursiv hineinsteigen
-- (sie werden als Knoten erfasst, aber ihre Kinder nicht gescannt)
shouldSkipDir :: FilePath -> Bool
shouldSkipDir rel = any (`isPrefixOf` rel) skipPrefixes
  where
    skipPrefixes =
      -- Build-Caches (riesig, aendern sich staendig)
      [ "dist-newstyle"
      , ".stack-work"
      -- Vendor (externe Projekte, nicht unsere Sache)
      , "IHaskell"
      , ".venv"
      -- IDE/Tool-Konfiguration (selten interessant)
      , ".git"
      ]


-- ═══════════════════════════════════════════════════════════
-- TEIL 2: NUIFIZIERUNG
-- 🐿️→⚡ nuifizieren :: [KobelNode] -> [KobelNode]
-- Jeder Knoten bekommt seine KOBEL-Kategorie zugewiesen
-- ═══════════════════════════════════════════════════════════

-- | 🐿️→⚡ nuifizieren: alle Knoten kategorisieren
categorizeAll :: [KobelNode] -> [KobelNode]
categorizeAll = map (\n -> n { knCategory = categorize n })

-- | Haupt-Kategorisierungsfunktion
-- Entscheidet fuer JEDEN Knoten im KOBEL, wohin er gehoert.
--
-- Prioritaet: Verzeichnis-Pfad > Dateiname-Muster > Extension > Fallback
categorize :: KobelNode -> KobelCategory
categorize node
  -- Verzeichnisse zuerst (haben andere Regeln als Dateien)
  | knIsDir node = categorizeDir (knRelPath node)
  -- Dateien: Pfad + Extension + Inhaltsmuster
  | otherwise    = categorizeFile (knRelPath node) ext
  where
    ext = map toLower $ takeExtension (knPath node)


-- ═══════════════════════════════════════════════════════════
-- TEIL 3: VERZEICHNIS-KATEGORISIERUNG
-- 📁 categorizeDir :: FilePath -> KobelCategory
-- Ordnet Top-Level- und verschachtelte Verzeichnisse ein
-- ═══════════════════════════════════════════════════════════

-- | Ordner kategorisieren
-- Die Top-Level-Ordner haben feste Zuordnungen basierend auf
-- der echten KOBEL-Struktur (Stand: 2026-03-05)
categorizeDir :: FilePath -> KobelCategory
categorizeDir rel
  -- ─── Quellcode-Verzeichnisse ───────────────────────
  | "app"     `isPrefixOf` rel = Verzeichnisbaum
  | "src"     `isPrefixOf` rel = Verzeichnisbaum

  -- ─── IDE/Tool-Konfiguration ────────────────────────
  | ".vscode"  `isPrefixOf` rel = Verzeichnisbaum
  | ".github"  `isPrefixOf` rel = Verzeichnisbaum
  | ".claude"  `isPrefixOf` rel = Verzeichnisbaum

  -- ─── Daten-Verzeichnisse ──────────────────────────
  | "data"           `isPrefixOf` rel = Datenbank
  | "archive_unpack" `isPrefixOf` rel = Datenbank
  | "txt"            `isPrefixOf` rel = Datenbank
  | ".snapshots"     `isPrefixOf` rel = Datenbank
  | "webextension.org" `isPrefixOf` rel = Datenbank

  -- ─── Chat-Exporte ─────────────────────────────────
  | "claude_code_sessions" `isPrefixOf` rel = ChatExport
  | "grok.com"             `isPrefixOf` rel = ChatExport

  -- ─── Dokumentation ─────────────────────────────────
  | "LINUX"          `isPrefixOf` rel = WeltDoku
  | "obsidian_vault" `isPrefixOf` rel = UserDoku
  | "docs"           `isPrefixOf` rel = UserDoku

  -- ─── Projekte ──────────────────────────────────────
  | "projekte" `isPrefixOf` rel = Projekt
  | ".apy"     `isPrefixOf` rel = Projekt

  -- ─── Vendor / Externe ──────────────────────────────
  | "IHaskell" `isPrefixOf` rel = Vendored
  | ".venv"    `isPrefixOf` rel = Vendored

  -- ─── Build-Artefakte ───────────────────────────────
  | "dist-newstyle" `isPrefixOf` rel = Artefakt
  | ".stack-work"   `isPrefixOf` rel = Artefakt
  | "PerfLogs"      `isPrefixOf` rel = Artefakt

  -- ─── project/ ist komplex: Mischung aus allem ─────
  -- (wird als ChatExport markiert, Dateien darin werden
  --  individuell feiner kategorisiert)
  | "project"  `isPrefixOf` rel = ChatExport

  -- ─── Fallback ──────────────────────────────────────
  | otherwise = Unbekannt


-- ═══════════════════════════════════════════════════════════
-- TEIL 4: DATEI-KATEGORISIERUNG
-- 📄 categorizeFile :: FilePath -> String -> KobelCategory
-- Die Hauptlogik: Pfad-Muster, Dateiname-Muster, Extension
--
-- PRIORITAET (hoeher = wird zuerst geprueft):
--   1. Pfad-Praefix (wo liegt die Datei?)
--   2. Dateiname-Muster (wie heisst sie?)
--   3. Extension (was fuer ein Typ?)
--   4. Fallback → Unbekannt
-- ═══════════════════════════════════════════════════════════

-- | Dateien kategorisieren
categorizeFile :: FilePath -> String -> KobelCategory
categorizeFile rel ext

  -- ═══════════════════════════════════════════════════
  -- PRIORITAET 1: PFAD-BASIERTE REGELN
  -- Der Pfad verraet am meisten ueber den Zweck einer Datei
  -- ═══════════════════════════════════════════════════

  -- ─── data/emojifiles/hs/ ──────────────────────────
  -- ACHTUNG: Diese .hs Dateien sind KEINE Haskell-Programme!
  -- Sie sind haselified Legal Documents (Beschwerde, Klageschrift etc.)
  -- Erkennungsmerkmale: Dateien wie 00_AKTEN_INDEX.hs,
  -- 01_BESCHWERDE_BETREUUNGSGERICHT_v3.hs, CLI.hs (Gesetzestext)
  | "data/emojifiles/" `isPrefixOf` rel = Datenbank

  -- ─── data/✳ *.txt — Architektur-Dokumente ──────────
  -- Dateien mit ✳-Praefix sind Squirrel-OS-Spezifikationen
  -- z.B. "✳ Squirrel OS Architecture.txt"
  | "data/" `isPrefixOf` rel && hasStarPrefix (takeFileName rel) = WeltDoku

  -- ─── data/*.hasel — HASEL-Manifeste ────────────────
  | "data/" `isPrefixOf` rel && ext == ".hasel" = ModelDoku

  -- ─── data/ allgemein ──────────────────────────────
  | "data/" `isPrefixOf` rel = Datenbank

  -- ─── src/Hasel/ — HASEL-Pipeline-Module ───────────
  | "src/Hasel/" `isPrefixOf` rel && ext == ".hs" = Verzeichnisbaum

  -- ─── src/LLM/ — LLM-Integration ──────────────────
  | "src/LLM/" `isPrefixOf` rel = Verzeichnisbaum

  -- ─── src/ Dokumentation ────────────────────────────
  | "src/" `isPrefixOf` rel && ext == ".md"  = WeltDoku
  | "src/" `isPrefixOf` rel && ext == ".mmd" = WeltDoku
  | "src/" `isPrefixOf` rel && ext == ".ps1" = Projekt
  | "src/" `isPrefixOf` rel                  = Verzeichnisbaum

  -- ─── app/ — Haskell-Einstiegspunkte ───────────────
  | "app/" `isPrefixOf` rel = Verzeichnisbaum

  -- ═══════════════════════════════════════════════════
  -- project/ — DAS GROSSE DURCHEINANDER
  -- Enthaelt: Chat-Exporte, Rechtsdokumente, Notion-Exporte,
  -- Bilder, Videos, PDFs, JSON-Daten, Skripte, alles.
  -- Hier muss dateiweise entschieden werden.
  -- ═══════════════════════════════════════════════════

  -- ─── project/ChatGPT-*.md — ChatGPT-Exporte ───────
  | "project/" `isPrefixOf` rel && "ChatGPT" `isInfixOf` fn = ChatExport

  -- ─── project/Claude-*.md — Claude-Exporte ─────────
  | "project/" `isPrefixOf` rel && "Claude" `isPrefixOf` fn = ChatExport

  -- ─── project/chat-export*.md — generische Exporte ──
  | "project/" `isPrefixOf` rel && "chat-export" `isPrefixOf` fnLower = ChatExport
  | "project/" `isPrefixOf` rel && "chat_export" `isPrefixOf` fnLower = ChatExport

  -- ─── project/artefakt_*.md — Session-Artefakte ─────
  | "project/" `isPrefixOf` rel && "artefakt" `isPrefixOf` fnLower = ChatExport

  -- ─── project/ Grok-Exporte ────────────────────────
  | "project/" `isPrefixOf` rel && "Grok" `isInfixOf` fn = ChatExport

  -- ─── project/ Notion-Exporte (UUID.html) ──────────
  -- Erkennungsmerkmal: Dateiname ist UUID + .html
  | "project/" `isPrefixOf` rel && ext == ".html" && looksLikeUUID fnNoExt = ChatExport

  -- ─── project/ GPT-Store-Seiten (g-*.html) ─────────
  | "project/" `isPrefixOf` rel && ext == ".html" && "g-" `isPrefixOf` fn = ChatExport

  -- ─── project/ Rechtsdokumente ─────────────────────
  -- Beschwerde, Klageschrift, Strafanzeige, Querella, Denuncia, Brief
  | "project/" `isPrefixOf` rel && isLegalDoc fn = UserDoku

  -- ─── project/ Index-Dokumente ─────────────────────
  | "project/" `isPrefixOf` rel && "_INDEX" `isInfixOf` fn = UserDoku

  -- ─── project/ PowerShell-Skripte ──────────────────
  | "project/" `isPrefixOf` rel && ext == ".ps1" = Projekt

  -- ─── project/ JSON-Daten (chunks, db-exports) ─────
  | "project/" `isPrefixOf` rel && ext == ".json" = Datenbank

  -- ─── project/ Excel/Tabellen ──────────────────────
  | "project/" `isPrefixOf` rel && ext == ".xlsx" = Datenbank
  | "project/" `isPrefixOf` rel && ext == ".csv"  = Datenbank
  | "project/" `isPrefixOf` rel && ext == ".sqlite" = Datenbank

  -- ─── project/ Bilder ──────────────────────────────
  | "project/" `isPrefixOf` rel && isImageExt ext = Datenbank

  -- ─── project/ Videos ──────────────────────────────
  | "project/" `isPrefixOf` rel && isVideoExt ext = Datenbank

  -- ─── project/ PDFs und Office-Docs ─────────────────
  | "project/" `isPrefixOf` rel && ext == ".pdf"  = UserDoku
  | "project/" `isPrefixOf` rel && ext == ".docx" = UserDoku
  | "project/" `isPrefixOf` rel && ext == ".ppt"  = Datenbank

  -- ─── project/ EXE-Dateien ─────────────────────────
  | "project/" `isPrefixOf` rel && ext == ".exe"  = Artefakt

  -- ─── project/ allgemeine HTML ─────────────────────
  | "project/" `isPrefixOf` rel && ext == ".html" = ChatExport

  -- ─── project/ Markdown (restliche) ─────────────────
  | "project/" `isPrefixOf` rel && ext == ".md"   = ChatExport

  -- ─── project/ Sonstiges ───────────────────────────
  | "project/" `isPrefixOf` rel = Datenbank

  -- ═══════════════════════════════════════════════════
  -- projekte/ — Build-Projekte und Skripte
  -- ═══════════════════════════════════════════════════

  | "projekte/" `isPrefixOf` rel && ext `elem` [".exe", ".hi", ".o"] = Artefakt
  | "projekte/" `isPrefixOf` rel && ext == ".cabal"  = Projekt
  | "projekte/" `isPrefixOf` rel && ext == ".nu"     = Projekt
  | "projekte/" `isPrefixOf` rel && ext == ".py"     = Projekt
  | "projekte/" `isPrefixOf` rel && ext == ".ps1"    = Projekt
  | "projekte/" `isPrefixOf` rel && ext == ".yaml"   = Projekt
  | "projekte/" `isPrefixOf` rel && ext == ".md"     = UserDoku
  | "projekte/" `isPrefixOf` rel && ext == ".docx"   = UserDoku
  | "projekte/" `isPrefixOf` rel && ext == ".zip"    = Datenbank
  | "projekte/" `isPrefixOf` rel = Projekt

  -- ═══════════════════════════════════════════════════
  -- claude_code_sessions/ — alles ChatExport
  -- ═══════════════════════════════════════════════════
  | "claude_code_sessions/" `isPrefixOf` rel = ChatExport

  -- ═══════════════════════════════════════════════════
  -- archive_unpack/ — WhatsApp/Messenger-Exporte
  -- ═══════════════════════════════════════════════════
  | "archive_unpack/" `isPrefixOf` rel = Datenbank

  -- ═══════════════════════════════════════════════════
  -- txt/ — Textdateien-Sammlung
  -- ═══════════════════════════════════════════════════
  | "txt/" `isPrefixOf` rel = Datenbank

  -- ═══════════════════════════════════════════════════
  -- .vscode/, .github/, .claude/ — Konfiguration
  -- ═══════════════════════════════════════════════════
  | ".vscode/" `isPrefixOf` rel  = Verzeichnisbaum
  | ".github/" `isPrefixOf` rel  = Verzeichnisbaum
  | ".claude/" `isPrefixOf` rel  = Verzeichnisbaum
  | ".snapshots/" `isPrefixOf` rel = Datenbank

  -- ═══════════════════════════════════════════════════
  -- PRIORITAET 2: DATEINAME-MUSTER (pfadunabhaengig)
  -- ═══════════════════════════════════════════════════

  -- ─── HASEL-Manifeste ──────────────────────────────
  | ext == ".hasel" = ModelDoku

  -- ─── Architektur-Dokumente (✳ Praefix) ────────────
  | hasStarPrefix fn = WeltDoku

  -- ─── Eichhoernchen-OS-Dokumente ───────────────────
  | "eichhornchen-os" `isInfixOf` fnLower = WeltDoku
  | "squirrel-os"     `isInfixOf` fnLower = WeltDoku
  | "Architecture"    `isInfixOf` fn       = WeltDoku

  -- ═══════════════════════════════════════════════════
  -- PRIORITAET 3: EXTENSION-BASIERTE REGELN
  -- Fallback wenn kein Pfad-Muster gegriffen hat
  -- ═══════════════════════════════════════════════════

  -- ─── Quellcode ────────────────────────────────────
  | ext == ".hs"   = Verzeichnisbaum
  | ext == ".jsx"  = Verzeichnisbaum
  | ext == ".tsx"  = Verzeichnisbaum
  | ext == ".js"   = Verzeichnisbaum
  | ext == ".ts"   = Verzeichnisbaum
  | ext == ".rs"   = Verzeichnisbaum

  -- ─── Skripte ──────────────────────────────────────
  | ext == ".ps1"  = Projekt
  | ext == ".py"   = Projekt
  | ext == ".nu"   = Projekt
  | ext == ".sh"   = Projekt

  -- ─── Build-Artefakte ──────────────────────────────
  | ext `elem` [".exe", ".hi", ".o", ".dll"] = Artefakt
  | ext == ".vhd"  = Artefakt
  | ext == ".vhdx" = Artefakt

  -- ─── Dokumentation ────────────────────────────────
  | ext == ".md"   = UserDoku
  | ext == ".pdf"  = UserDoku
  | ext == ".docx" = UserDoku
  | ext == ".txt"  = UserDoku

  -- ─── Konfiguration ────────────────────────────────
  | ext == ".json"  = Verzeichnisbaum
  | ext == ".yaml"  = Verzeichnisbaum
  | ext == ".yml"   = Verzeichnisbaum
  | ext == ".toml"  = Verzeichnisbaum
  | ext == ".cabal" = Projekt
  | ext == ".lock"  = Verzeichnisbaum

  -- ─── Daten/Archive ────────────────────────────────
  | ext == ".zip"     = Datenbank
  | ext == ".tar"     = Datenbank
  | ext == ".gz"      = Datenbank
  | ext == ".sqlite"  = Datenbank
  | ext == ".sqlite3" = Datenbank
  | ext == ".csv"     = Datenbank
  | ext == ".xlsx"    = Datenbank

  -- ─── Bilder/Medien ────────────────────────────────
  | isImageExt ext = Datenbank
  | isVideoExt ext = Datenbank

  -- ─── Sonstiges ────────────────────────────────────
  | ext == ".html" = ChatExport
  | ext == ".env"  = Verzeichnisbaum

  -- ═══════════════════════════════════════════════════
  -- FALLBACK
  -- ═══════════════════════════════════════════════════
  | otherwise = Unbekannt

  where
    fn      = takeFileName rel
    fnLower = map toLower fn
    fnNoExt = takeWhile (/= '.') fn


-- ═══════════════════════════════════════════════════════════
-- TEIL 5: HILFSFUNKTIONEN
-- Muster-Erkennung fuer Dateinamen und Pfade
-- ═══════════════════════════════════════════════════════════

-- | Erkennt Dateien mit ✳-Praefix (Squirrel-OS-Architektur-Dokumente)
-- Beispiele: "✳ Squirrel OS Architecture.txt", "✳ Z Emojifier.txt"
hasStarPrefix :: String -> Bool
hasStarPrefix name =
  "✳" `isPrefixOf` name ||            -- UTF-8 Stern
  "\10035" `isPrefixOf` name ||        -- ✳ als Escape
  "* " `isPrefixOf` name              -- ASCII Fallback (selten)

-- | Erkennt Rechtsdokumente anhand des Dateinamens
-- Diese Dateien gehoeren in UserDoku, nicht ChatExport
isLegalDoc :: String -> Bool
isLegalDoc name = any (`isInfixOf` nameUpper) legalKeywords
  where
    nameUpper = map toLower name  -- eigentlich case-insensitive
    legalKeywords =
      [ "beschwerde"
      , "klageschrift"
      , "strafanzeige"
      , "querella"
      , "denuncia"
      , "brief_ra"
      , "begleitschreiben"
      , "beweismittel"
      , "boot_report"
      ]

-- | Prueft ob ein String wie eine UUID aussieht
-- UUIDs: 8-4-4-4-12 Hex-Zeichen, z.B. "a1b2c3d4-e5f6-7890-abcd-ef1234567890"
-- Auch Notion-IDs ohne Bindestriche: 32+ Hex-Zeichen
looksLikeUUID :: String -> Bool
looksLikeUUID s
  -- Standard UUID: 8-4-4-4-12 = 36 Zeichen mit Bindestrichen
  | length s == 36 && all (\c -> isHexDigit c || c == '-') s = True
  -- Kompakte UUID: 32 Hex-Zeichen ohne Bindestriche
  | length s == 32 && all isHexDigit s = True
  -- Notion-Style: lange Hex-Strings (mindestens 20 Zeichen)
  | length s >= 20 && all (\c -> isHexDigit c || c == '-') s = True
  | otherwise = False

-- | Bild-Erweiterungen
isImageExt :: String -> Bool
isImageExt ext = ext `elem`
  [ ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".svg"
  , ".webp", ".ico", ".tiff", ".heic"
  ]

-- | Video-Erweiterungen
isVideoExt :: String -> Bool
isVideoExt ext = ext `elem`
  [ ".mp4", ".mkv", ".avi", ".mov", ".webm"
  , ".flv", ".wmv", ".m4v"
  ]


-- ═══════════════════════════════════════════════════════════
-- TEIL 6: STATISTIK-HELFER (fuer Pipeline-Ausgabe)
-- ═══════════════════════════════════════════════════════════

-- | Zaehlt Knoten pro Kategorie
-- Nuetzlich fuer die Pipeline-Ausgabe und Manifest-Generierung
--
-- Beispiel-Ausgabe nach categorizeAll auf dem echten KOBEL:
--
--   🌲 Verzeichnisbaum:  ~40  (app/, src/, .vscode/, .github/)
--   💾 Datenbank:        ~200 (data/emojifiles/, archive_unpack/, zips)
--   🏖️ UserDoku:         ~30  (project/legal docs, READMEs)
--   🗺️ WeltDoku:          ~10  (✳ Architecture, eichhornchen-os)
--   🐿️ ModelDoku:          ~2  (sciuridae-manifest.hasel)
--   📝 ChatExport:       ~300 (project/ChatGPT-*, Claude-*, Notion)
--   🔨 Projekt:           ~30  (projekte/*.nu, .py, .ps1)
--   📦 Vendored:           ~2  (IHaskell/, .venv/)
--   ⚙️ Artefakt:          ~50  (projekte/*.exe, dist-newstyle/)
--   ❓ Unbekannt:          ~5  (Sonderfaelle)
--
-- GESAMT:               ~670 Knoten
--
-- ∎ Ende der KOBEL-Erkenntnisse
-- λ 🐿️ → 🌳 (🐿️ × 🥥(🌰🌰🌰🌰🌰🌰)) ∧ (¬🦊) ∧ (¬🐍)

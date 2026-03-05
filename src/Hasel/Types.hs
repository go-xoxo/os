{-# LANGUAGE OverloadedStrings #-}

-- ✳ HASEL TYPES — Schicht 3 (ASCII/Haskell)
-- 🐿️ Typen für die KOBEL-Reorganisation
-- λ Types → 🌰 ⊕ 🪺 { KobelNode, Category, FreezeResult }

module Hasel.Types where

import Data.Text (Text)

-- | 🐿️ Subfamilien der Sciuridae (5 Familien, 284 Arten)
data Subfamily
  = Callosciurinae   -- 🌴 67 Arten, Asiatische Hörnchen
  | Ratufinae        -- 🦁 4 Arten, Riesenhörnchen
  | Sciurillinae     -- 🤏 1 Art, Neotropisches Zwerghörnchen
  | Sciurinae        -- 🌳 84 Arten, Baumhörnchen + Flughörnchen
  | Xerinae          -- 🕳️ 128 Arten, Erdhörnchen + Murmeltiere
  deriving (Show, Eq, Ord)

-- | 🌳 Kategorien im KOBEL (Verzeichnisbaum-Ontologie)
data KobelCategory
  = Verzeichnisbaum  -- 🌲 Quellcode, Haskell-Module
  | Datenbank        -- 💾 Archivierte Daten, ZIPs, Binaries
  | UserDoku         -- 🏖️ Benutzer-Dokumentation (README pro Ordner)
  | WeltDoku         -- 🗺️ Architektur, OS-Spezifikation
  | ModelDoku        -- 🐿️ HASEL-Selbstdokumentation (.hasel Dateien)
  | ChatExport       -- 📝 Exportierte Chat-Sitzungen
  | Projekt          -- 🔨 Projekte, Tools, Skripte
  | Vendored         -- 📦 Externe Abhängigkeiten (IHaskell etc.)
  | Artefakt         -- ⚙️ Build-Artefakte (.exe, .hi, .o)
  | Unbekannt        -- ❓ Nicht klassifiziert
  deriving (Show, Eq, Ord)

-- | 🌰 Ein Knoten im KOBEL-Baum
data KobelNode = KobelNode
  { knPath     :: FilePath     -- 🚪 Absoluter Pfad
  , knRelPath  :: FilePath     -- 🚪 Relativer Pfad zum KOBEL-Root
  , knIsDir    :: Bool         -- 📁 Ordner oder Datei?
  , knSize     :: Integer      -- 📏 Dateigröße in Bytes
  , knCategory :: KobelCategory -- 🌳 Klassifikation
  } deriving (Show, Eq)

-- | 💪 Ein Befehl zur Reorganisation
data MoveCommand
  = MkDir   FilePath           -- mkdir -p
  | Move    FilePath FilePath  -- mv src dst
  | Copy    FilePath FilePath  -- cp src dst (für Doku)
  | GenFile FilePath Text      -- generierte Datei schreiben
  | Skip    FilePath Text      -- überspringen mit Grund
  deriving (Show, Eq)

-- | 🔒 Pipeline-Modus: DryRun zeigt nur was passieren würde
data PipelineMode
  = DryRun    -- 🔍 Nur anzeigen, nichts verändern
  | Execute   -- 🚀 Tatsächlich ausführen
  deriving (Show, Eq)

-- | ❄️ Gefriergetrocknetes Ergebnis
data FreezeResult = FreezeResult
  { frCommands  :: [MoveCommand]  -- ausgeführte Befehle
  , frMoved     :: Int            -- 🚪 verschobene Dateien
  , frCreated   :: Int            -- 📁 erstellte Ordner
  , frGenerated :: Int            -- 📝 generierte Dateien
  , frSkipped   :: Int            -- ⏭️ übersprungene Dateien
  , frManifest  :: Text           -- ❄️ destilliertes Manifest
  , frMode      :: PipelineMode   -- 🔒 Modus (DryRun/Execute)
  } deriving (Show)

-- | 🏷️ Emoji-Zuordnung für Kategorien
categoryEmoji :: KobelCategory -> Text
categoryEmoji Verzeichnisbaum = "🌲"
categoryEmoji Datenbank       = "💾"
categoryEmoji UserDoku        = "🏖️"
categoryEmoji WeltDoku        = "🗺️"
categoryEmoji ModelDoku       = "🐿️"
categoryEmoji ChatExport      = "📝"
categoryEmoji Projekt         = "🔨"
categoryEmoji Vendored        = "📦"
categoryEmoji Artefakt        = "⚙️"
categoryEmoji Unbekannt       = "❓"

-- | 🏷️ Zielordner für jede Kategorie
categoryTargetDir :: KobelCategory -> FilePath
categoryTargetDir Verzeichnisbaum = "src"
categoryTargetDir Datenbank       = "data"
categoryTargetDir UserDoku        = "docs/user"
categoryTargetDir WeltDoku        = "docs/world"
categoryTargetDir ModelDoku       = "docs/model"
categoryTargetDir ChatExport      = "chats"
categoryTargetDir Projekt         = "projekte"
categoryTargetDir Vendored        = "vendor"
categoryTargetDir Artefakt        = "build"
categoryTargetDir Unbekannt       = "unsorted"

-- | 🛡️ Geschützte Pfade — Dateien hier werden NIE verschoben
-- Diese Verzeichnisse sind bereits am richtigen Ort im KOBEL.
protectedPrefixes :: [String]
protectedPrefixes =
  [ "app/"             -- 🚪 Haskell Einstiegspunkt
  , "src/"             -- 🌲 Quellcode (Hasel, LLM)
  , ".vscode/"         -- ⚙️ IDE-Konfiguration
  , ".github/"         -- 🐙 GitHub Actions/Config
  , ".claude/"         -- 🤖 Claude Config
  , "projekte/"        -- 🔨 Projekt-Configs (.cabal)
  , "IHaskell/"        -- 📦 Vendored (IHaskell)
  , ".stack-work/"     -- ⚙️ Build-Cache
  , "dist-newstyle/"   -- ⚙️ Cabal-Build-Cache
  , "node_modules/"    -- 📦 JS-Abhängigkeiten
  ]

-- | 🛡️ Geschützte Root-Dateien — werden NIE verschoben
protectedRootFiles :: [String]
protectedRootFiles =
  [ "README.md"
  , "CHANGELOG.md"
  , "LICENSE"
  , ".gitignore"
  , "package.json"
  , "bun.lockb"
  , "tsconfig.json"
  , "index.jsx"
  , "Cargo.toml"
  , "Cargo.lock"
  , "fourmolu.yaml"
  , "squirrel-os.cabal"
  , "CLAUDE.md"
  ]

-- | 🛡️ Ist dieser relative Pfad geschützt?
isProtectedPath :: FilePath -> Bool
isProtectedPath relPath =
  -- Root-Dateien (kein / im Pfad) sind geschützt
  notElem '/' relPath
  -- Bekannte Root-Dateien explizit
  || relPath `elem` protectedRootFiles
  -- Dateien in geschützten Verzeichnissen
  || any (`isPrefixOfStr` relPath) protectedPrefixes
  where
    isPrefixOfStr [] _          = True
    isPrefixOfStr _ []          = False
    isPrefixOfStr (x:xs) (y:ys) = x == y && isPrefixOfStr xs ys

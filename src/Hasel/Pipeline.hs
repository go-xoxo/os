{-# LANGUAGE OverloadedStrings #-}

-- ✳ HASEL PIPELINE — Schicht 3 (ASCII/Haskell)
-- 🔄 pipeline = haselifizieren >=> nuifizieren >=> powershellen >=> adaptieren >=> gefriertrocknen
-- 🌰→🐿️→⚡→💪→🚪→❄️

module Hasel.Pipeline where

import Hasel.Types
import Hasel.Kernel (filterFuchs, filterSchlange, printStats, freezeToText)
import Hasel.Kobel (scanKobel, categorizeAll)
import Hasel.Readme (generateReadme)

import Control.Monad (forM_, unless)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , renamePath
    )
import System.FilePath ((</>), takeDirectory)

-- | 🌰→🐿️ HASELIFIZIEREN: Scan the KOBEL tree
haselifizieren :: FilePath -> IO [KobelNode]
haselifizieren root = do
  TIO.putStrLn "🌰→🐿️ haselifizieren :: scanKobel..."
  nodes <- scanKobel root
  let safe = filterSchlange (filterFuchs nodes)
  printStats safe
  return safe

-- | 🐿️→⚡ NUIFIZIEREN: Classify every node
nuifizieren :: [KobelNode] -> IO [KobelNode]
nuifizieren nodes = do
  TIO.putStrLn "🐿️→⚡ nuifizieren :: categorizeAll..."
  let categorized = categorizeAll nodes
      stats = Map.fromListWith (+) [(knCategory n, 1 :: Int) | n <- categorized]
  forM_ (Map.toList stats) $ \(cat, count) ->
    TIO.putStrLn $ T.concat
      ["  ", categoryEmoji cat, " ", T.pack (show cat), ": ", T.pack (show count)]
  return categorized

-- | ⚡→💪 POWERSHELLEN: Generate move commands
powershellen :: FilePath -> [KobelNode] -> IO [MoveCommand]
powershellen root nodes = do
  TIO.putStrLn "⚡→💪 powershellen :: generateCommands..."
  let fileNodes = filter (not . knIsDir) nodes
      cmds = concatMap (nodeToCommands root) fileNodes
      -- Auch README-Generierung für Zielordner
      targetDirs = nub [categoryTargetDir (knCategory n) | n <- nodes]
      mkdirs = [MkDir (root </> d) | d <- targetDirs]
      readmes = generateReadmeCommands root nodes
  return (mkdirs ++ cmds ++ readmes)

-- | Einen Knoten in Befehle umwandeln
nodeToCommands :: FilePath -> KobelNode -> [MoveCommand]
nodeToCommands root node =
  case knCategory node of
    -- 🛡️ Geschützte Pfade: NIEMALS verschieben
    _ | isProtectedPath (knRelPath node) -> [Skip (knPath node) "geschützter Pfad"]
    -- Dateien die schon am richtigen Ort sind: überspringen
    _ | isAlreadyInPlace node -> [Skip (knPath node) "bereits am Zielort"]
    -- Build-Artefakte: überspringen (nicht verschieben)
    Artefakt  -> [Skip (knPath node) "Build-Artefakt"]
    -- Vendored: überspringen
    Vendored  -> [Skip (knPath node) "vendored"]
    -- Alles andere: verschieben
    _         -> [Move (knPath node) (targetPath root node)]

-- | Ist der Knoten bereits im Zielverzeichnis?
isAlreadyInPlace :: KobelNode -> Bool
isAlreadyInPlace node =
  let target = categoryTargetDir (knCategory node)
      rel    = knRelPath node
  in target `isPrefixOfPath` rel
  where
    isPrefixOfPath prefix path = (prefix ++ "/") `strIsPrefixOf` path
                              || prefix == path
    strIsPrefixOf [] _          = True
    strIsPrefixOf _ []          = False
    strIsPrefixOf (x:xs) (y:ys) = x == y && strIsPrefixOf xs ys

-- | Zielpfad berechnen — bewahrt Unterverzeichnis-Struktur
targetPath :: FilePath -> KobelNode -> FilePath
targetPath root node =
  root </> categoryTargetDir (knCategory node) </> preserveSubpath (knRelPath node)
  where
    -- Bewahre den relativen Pfad ab der ersten Ebene
    -- z.B. "data/emojifiles/hs/foo.hs" → "emojifiles/hs/foo.hs"
    -- z.B. "foo.hs" (root-level) → "foo.hs"
    preserveSubpath p = case break (== '/') p of
      (_, '/':rest) -> if null rest then takeFileName' p else rest
      _             -> takeFileName' p
    takeFileName' p = case break (== '/') (reverse p) of
      (name, _) -> reverse name

-- | 💪→🚪 ADAPTIEREN: Pfade an Windows anpassen + Dry-Run-Ausgabe
adaptieren :: [MoveCommand] -> IO [MoveCommand]
adaptieren cmds = do
  TIO.putStrLn "💪→🚪 adaptieren :: validatePaths..."
  let valid = filter isValid cmds
  TIO.putStrLn $ T.concat
    [ "  ", T.pack (show (length valid)), " Befehle validiert"
    , " (", T.pack (show (length cmds - length valid)), " ungültig verworfen)"
    ]
  return valid
  where
    isValid (Skip _ _) = True
    isValid (MkDir _)  = True
    isValid (Move s d)  = s /= d
    isValid (Copy s d)  = s /= d
    isValid (GenFile _ _) = True

-- | 🚪→❄️ GEFRIERTROCKNEN: Execute commands + produce manifest
gefriertrocknen :: PipelineMode -> [MoveCommand] -> IO FreezeResult
gefriertrocknen mode cmds = do
  case mode of
    DryRun -> do
      TIO.putStrLn "🚪→❄️ gefriertrocknen :: DRY-RUN (keine Änderungen)..."
      TIO.putStrLn ""
      -- Im DryRun nur die Move-Befehle anzeigen
      let moves = [(s,d) | Move s d <- cmds]
          mkdirs' = [d | MkDir d <- cmds]
          gens   = [p | GenFile p _ <- cmds]
      TIO.putStrLn $ T.concat ["  📁 ", T.pack (show (length mkdirs')), " Verzeichnisse würden erstellt"]
      TIO.putStrLn $ T.concat ["  🚪 ", T.pack (show (length moves)), " Dateien würden verschoben"]
      TIO.putStrLn $ T.concat ["  📝 ", T.pack (show (length gens)), " Dateien würden generiert"]
      TIO.putStrLn ""
      -- Zeige die ersten 20 Moves als Vorschau
      let preview = take 20 moves
      forM_ preview $ \(s, d) ->
        TIO.putStrLn $ T.concat ["  🔍 ", T.pack s, " → ", T.pack d]
      if length moves > 20
        then TIO.putStrLn $ T.concat ["  ... und ", T.pack (show (length moves - 20)), " weitere"]
        else return ()
    Execute -> do
      TIO.putStrLn "🚪→❄️ gefriertrocknen :: EXECUTE..."
      counts <- mapM executeCmd cmds
      let moved   = length [() | CmdMoved   <- counts]
          created = length [() | CmdCreated <- counts]
          genned  = length [() | CmdGenned  <- counts]
          skipped = length [() | CmdSkipped <- counts]
      TIO.putStrLn $ T.concat ["  ✅ ", T.pack (show moved), " verschoben, ",
                                T.pack (show created), " erstellt, ",
                                T.pack (show genned), " generiert, ",
                                T.pack (show skipped), " übersprungen"]
  -- Ergebnis zusammenbauen (für beide Modi)
  let moves   = length [() | Move _ _ <- cmds]
      mkdirs' = length [() | MkDir _  <- cmds]
      gens    = length [() | GenFile _ _ <- cmds]
      skips   = length [() | Skip _ _ <- cmds]
      manifest = buildManifest cmds
      result = FreezeResult
        { frCommands  = cmds
        , frMoved     = if mode == DryRun then 0 else moves
        , frCreated   = if mode == DryRun then 0 else mkdirs'
        , frGenerated = if mode == DryRun then 0 else gens
        , frSkipped   = skips
        , frManifest  = manifest
        , frMode      = mode
        }
  TIO.putStrLn (freezeToText result)
  return result

data CmdResult = CmdMoved | CmdCreated | CmdGenned | CmdSkipped

-- | Einzelnen Befehl ausführen
executeCmd :: MoveCommand -> IO CmdResult
executeCmd (MkDir dir) = do
  createDirectoryIfMissing True dir
  return CmdCreated
executeCmd (Move src dst) = do
  let dstDir = takeDirectory dst
  createDirectoryIfMissing True dstDir
  srcExists <- doesFileExist src
  dstExists <- doesFileExist dst
  if srcExists && not dstExists
    then do
      renamePath src dst
      TIO.putStrLn $ T.concat ["  🚪 ", T.pack src, " → ", T.pack dst]
      return CmdMoved
    else return CmdSkipped
executeCmd (Copy src dst) = do
  let dstDir = takeDirectory dst
  createDirectoryIfMissing True dstDir
  srcExists <- doesFileExist src
  dstExists <- doesFileExist dst
  if srcExists && not dstExists
    then do
      contents <- readFile src
      writeFile dst contents
      return CmdMoved
    else return CmdSkipped
executeCmd (GenFile path content) = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  exists <- doesFileExist path
  unless exists $ TIO.writeFile path content
  return CmdGenned
executeCmd (Skip _ _) =
  return CmdSkipped

-- | README-Befehle für alle Zielordner generieren
generateReadmeCommands :: FilePath -> [KobelNode] -> [MoveCommand]
generateReadmeCommands root nodes =
  let byCategory = Map.fromListWith (++) [(knCategory n, [n]) | n <- nodes]
      mkReadme (cat, ns) =
        let dir = root </> categoryTargetDir cat
            content = generateReadme cat ns
        in GenFile (dir </> "README.md") content
  in map mkReadme (Map.toList byCategory)

-- | ❄️ Manifest aus Befehlen destillieren
buildManifest :: [MoveCommand] -> T.Text
buildManifest cmds = T.unlines $
  [ "-- ❄️ KOBEL MANIFEST — gefriergetrocknet"
  , "-- λ manifest → 🪺 {"
  ] ++
  map (\l -> T.concat ["--   ", l]) (summarize cmds) ++
  [ "-- } ∎" ]
  where
    summarize cs =
      let moves   = [() | Move _ _ <- cs]
          mkdirs  = [() | MkDir _ <- cs]
          gens    = [() | GenFile _ _ <- cs]
          skips   = [() | Skip _ _ <- cs]
      in [ T.concat ["🚪 Move:    ", T.pack (show (length moves))]
         , T.concat ["📁 MkDir:   ", T.pack (show (length mkdirs))]
         , T.concat ["📝 GenFile: ", T.pack (show (length gens))]
         , T.concat ["⏭️  Skip:    ", T.pack (show (length skips))]
         ]

-- | 🔄 Der komplette Pipeline: haselifizieren >=> nuifizieren >=> powershellen >=> adaptieren >=> gefriertrocknen
pipeline :: PipelineMode -> FilePath -> IO FreezeResult
pipeline mode root = do
  TIO.putStrLn "═══════════════════════════════════════════════"
  TIO.putStrLn "🐿️ KOBEL REORGANISATION — HASEL PIPELINE v1.0"
  case mode of
    DryRun  -> TIO.putStrLn "🔒 MODUS: DRY-RUN (keine Änderungen)"
    Execute -> TIO.putStrLn "🚀 MODUS: EXECUTE (Dateien werden verschoben!)"
  TIO.putStrLn "═══════════════════════════════════════════════"
  nodes    <- haselifizieren root
  classified <- nuifizieren nodes
  cmds     <- powershellen root classified
  adapted  <- adaptieren cmds
  result   <- gefriertrocknen mode adapted
  TIO.putStrLn "═══════════════════════════════════════════════"
  TIO.putStrLn "🐿️ λ selbst → selbst selbst >>= ∎"
  return result

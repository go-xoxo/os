{-# LANGUAGE OverloadedStrings #-}

-- ✳ HASEL PIPELINE — Schicht 3 (ASCII/Haskell)
-- 🔄 pipeline = haselifizieren >=> nuifizieren >=> powershellen >=> adaptieren >=> gefriertrocknen
-- 🌰→🐿️→⚡→💪→🚪→❄️

module Hasel.Pipeline where

import Hasel.Types
import Hasel.Kernel (filterFuchs, filterSchlange, printStats, freezeToText)
import Hasel.Kobel (scanKobel, categorizeAll)
import Hasel.Readme (generateReadme)

import Control.Monad (forM_, when, unless)
import Data.List (nub, group, sort, sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
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
    -- Dateien die schon am richtigen Ort sind: überspringen
    _ | isAlreadyInPlace root node -> [Skip (knPath node) "bereits am Zielort"]
    -- Build-Artefakte: überspringen (nicht verschieben)
    Artefakt  -> [Skip (knPath node) "Build-Artefakt"]
    -- Vendored: überspringen
    Vendored  -> [Skip (knPath node) "vendored"]
    -- Alles andere: verschieben
    cat       -> [Move (knPath node) (targetPath root node)]

-- | Ist der Knoten bereits im Zielverzeichnis?
isAlreadyInPlace :: FilePath -> KobelNode -> Bool
isAlreadyInPlace _ node =
  let target = categoryTargetDir (knCategory node)
      rel    = knRelPath node
  in target `isPrefixOfPath` rel
  where
    isPrefixOfPath prefix path = (prefix ++ "/") `isPrefixOf` path
                              || prefix == path
    isPrefixOf [] _          = True
    isPrefixOf _ []          = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- | Zielpfad berechnen
targetPath :: FilePath -> KobelNode -> FilePath
targetPath root node =
  root </> categoryTargetDir (knCategory node) </> takeFileName' (knRelPath node)
  where
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
gefriertrocknen :: [MoveCommand] -> IO FreezeResult
gefriertrocknen cmds = do
  TIO.putStrLn "🚪→❄️ gefriertrocknen :: execute..."
  counts <- mapM executeCmd cmds
  let moved   = length [() | CmdMoved   <- counts]
      created = length [() | CmdCreated <- counts]
      genned  = length [() | CmdGenned  <- counts]
      skipped = length [() | CmdSkipped <- counts]
      manifest = buildManifest cmds
  let result = FreezeResult
        { frCommands  = cmds
        , frMoved     = moved
        , frCreated   = created
        , frGenerated = genned
        , frSkipped   = skipped
        , frManifest  = manifest
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
executeCmd (Skip path reason) = do
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
  [ T.concat ["--   ", describeCmds cmds'] | cmds' <- groupCmds cmds ] ++
  [ "-- } ∎" ]
  where
    groupCmds cs =
      let moves   = [() | Move _ _ <- cs]
          mkdirs  = [() | MkDir _ <- cs]
          gens    = [() | GenFile _ _ <- cs]
          skips   = [() | Skip _ _ <- cs]
      in [ T.concat ["🚪 Move:    ", T.pack (show (length moves))]
         , T.concat ["📁 MkDir:   ", T.pack (show (length mkdirs))]
         , T.concat ["📝 GenFile: ", T.pack (show (length gens))]
         , T.concat ["⏭️  Skip:    ", T.pack (show (length skips))]
         ]
    describeCmds = id  -- passthrough, already formatted

-- | 🔄 Der komplette Pipeline: haselifizieren >=> nuifizieren >=> powershellen >=> adaptieren >=> gefriertrocknen
pipeline :: FilePath -> IO FreezeResult
pipeline root = do
  TIO.putStrLn "═══════════════════════════════════════════════"
  TIO.putStrLn "🐿️ KOBEL REORGANISATION — HASEL PIPELINE v1.0"
  TIO.putStrLn "═══════════════════════════════════════════════"
  nodes    <- haselifizieren root
  classified <- nuifizieren nodes
  cmds     <- powershellen root classified
  adapted  <- adaptieren cmds
  result   <- gefriertrocknen adapted
  TIO.putStrLn "═══════════════════════════════════════════════"
  TIO.putStrLn "🐿️ λ selbst → selbst selbst >>= ∎"
  return result

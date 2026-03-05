{-# LANGUAGE OverloadedStrings #-}

-- ✳ HASEL MAIN — Schicht 3 (ASCII/Haskell)
-- 🐿️ Einstiegspunkt fuer die KOBEL-Reorganisation
-- λ Main → pipeline kobelRoot >>= ❄️
-- 🔒 Standard: DRY-RUN — nur mit --execute werden Dateien verschoben

module Main (main) where

import System.Environment (getArgs)
import System.Directory   (getCurrentDirectory)
import System.IO          (hSetEncoding, stdout, stderr, utf8)
import Data.Text          (Text, pack)
import qualified Data.Text.IO as TIO

import Hasel.Pipeline (pipeline)
import Hasel.Types    (PipelineMode(..), frMoved, frCreated, frSkipped, frGenerated, frManifest, frMode)

main :: IO ()
main = do
  -- 🐿️ UTF-8 erzwingen damit Emoji auf Windows-Konsolen funktionieren
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  let (mode, rootArg) = parseArgs args
  root <- case rootArg of
    Just r  -> return r
    Nothing -> getCurrentDirectory
  TIO.putStrLn "🌰 HASEL Pipeline startet..."
  TIO.putStrLn $ "📂 KOBEL root: " <> pack root
  result <- pipeline mode root
  TIO.putStrLn ""
  TIO.putStrLn "📊 Ergebnis:"
  case frMode result of
    DryRun -> do
      TIO.putStrLn "   🔒 DRY-RUN — keine Dateien wurden verändert"
      TIO.putStrLn "   💡 Benutze --execute um tatsächlich zu reorganisieren"
    Execute -> do
      TIO.putStrLn $ "   📦 Verschoben:  " <> showT (frMoved result)
      TIO.putStrLn $ "   📁 Erstellt:    " <> showT (frCreated result)
      TIO.putStrLn $ "   📝 Generiert:   " <> showT (frGenerated result)
  TIO.putStrLn $ "   ⏭️  Übersprungen: " <> showT (frSkipped result)
  TIO.putStrLn ""
  TIO.putStrLn "📋 Manifest:"
  TIO.putStrLn (frManifest result)
  TIO.putStrLn "❄️ Gefriergetrocknet. Fertig!"

-- | CLI-Argumente parsen: --execute für echte Ausführung, sonst DryRun
parseArgs :: [String] -> (PipelineMode, Maybe FilePath)
parseArgs args =
  let hasExecute = "--execute" `elem` args
      rest = filter (/= "--execute") args
      rootPath = case rest of
        [r] -> Just r
        _   -> Nothing
  in (if hasExecute then Execute else DryRun, rootPath)

showT :: Show a => a -> Text
showT = pack . show

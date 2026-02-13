{-# LANGUAGE OverloadedStrings #-}

-- | HASEL REPL — emoji go! 🐿️🚀
--
-- The interactive playground where squirrels of all ages
-- can crack nuts, stash values, and explore all FP universes.
module Hasel.Repl
  ( emojiGo
  , runRepl
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import System.IO (hFlush, stdout, hSetEncoding, utf8)

import Hasel.Core
import Hasel.Emoji
import Hasel.Universe

-- | Entry point — emoji go! 🐿️🚀
emojiGo :: IO ()
emojiGo = do
  hSetEncoding stdout utf8
  TIO.putStrLn banner
  TIO.putStrLn ""
  TIO.putStrLn "  Willkommen im HASEL Spielplatz! 🎮"
  TIO.putStrLn "  Tippe Ausdrücke oder Befehle ein."
  TIO.putStrLn ""
  TIO.putStrLn "  Befehle:"
  TIO.putStrLn "    :hilfe      — Zeige diese Hilfe"
  TIO.putStrLn "    :universen  — Zeige alle FP-Universen"
  TIO.putStrLn "    :kobel      — Zeige gespeicherte Nüsse"
  TIO.putStrLn "    :nuss NAME WERT — Speichere eine Nuss"
  TIO.putStrLn "    :knack AUSDRUCK — Werte einen Ausdruck aus"
  TIO.putStrLn "    :tschuess   — Verlasse den Spielplatz"
  TIO.putStrLn ""
  runRepl defaultKobel

-- | The HASEL banner 🐿️
banner :: Text
banner = T.unlines
  [ ""
  , "  🐿️ ╔═══════════════════════════════════════════╗"
  , "  🌰 ║                                           ║"
  , "  🌳 ║   H A S E L                                ║"
  , "  ☂️  ║   Haskell + Emoji Language                 ║"
  , "  ⚡ ║                                           ║"
  , "  💚 ║   All FP universes. One umbrella.          ║"
  , "  🎮 ║   Spass fuer Eichhoernchen jeden Alters.   ║"
  , "  🌰 ║                                           ║"
  , "  🐿️ ╚═══════════════════════════════════════════╝"
  ]

-- | Default Kobel with some pre-stashed nuts.
defaultKobel :: Kobel
defaultKobel = Map.fromList
  [ ("pi",       Zahl 3.14159265358979)
  , ("e",        Zahl 2.71828182845905)
  , ("antwort",  Zahl 42)
  , ("hallo",    Wort "Hallo, Eichhörnchen! 🐿️")
  , ("wahr",     Wahrheit True)
  , ("falsch",   Wahrheit False)
  , ("leer",     Nichts)
  ]

-- | The main REPL loop.
runRepl :: Kobel -> IO ()
runRepl env = do
  TIO.putStr "🐿️ > "
  hFlush stdout
  line <- TIO.getLine
  let trimmed = T.strip line
  if T.null trimmed
    then runRepl env
    else case processCommand trimmed env of
      Quit       -> TIO.putStrLn "🐿️ Tschüss! Bis bald im Wald! 🌳🌰"
      Continue e -> runRepl e
      Display t e -> do
        TIO.putStrLn t
        runRepl e

data ReplResult
  = Quit
  | Continue Kobel
  | Display Text Kobel

-- | Process a REPL command or expression.
processCommand :: Text -> Kobel -> ReplResult
processCommand input env
  | input == ":tschuess" || input == ":quit" || input == ":q"
    = Quit
  | input == ":hilfe" || input == ":help" || input == ":h"
    = Display helpText env
  | input == ":universen" || input == ":universes"
    = Display (showAllUniverses) env
  | input == ":kobel" || input == ":env"
    = Display (showKobel env) env
  | ":nuss " `T.isPrefixOf` input
    = processStash (T.drop 6 input) env
  | ":knack " `T.isPrefixOf` input
    = processCrack (T.drop 7 input) env
  | otherwise
    = processExpr input env

-- | Show all FP universes under the HASEL umbrella.
showAllUniverses :: Text
showAllUniverses = T.unlines $
  [ "", "  ☂️  HASEL importiert das Beste aus allen Welten:", "" ]
  ++ map formatUniverse allUniverses
  ++ [ "", "  🌰 Alle Nüsse. Ein Kobel. ☂️" ]

formatUniverse :: Universe -> Text
formatUniverse u =
  "    " <> showUniverse u <> "\n"
  <> "      " <> beschreibung u

-- | Show the contents of a Kobel.
showKobel :: Kobel -> Text
showKobel env
  | Map.null env = "  🏠 Der Kobel ist leer."
  | otherwise = T.unlines $
      [ "", "  🏠 Dein Kobel:" ]
      ++ map formatEntry (Map.toList env)

formatEntry :: (Text, Nuss) -> Text
formatEntry (k, v) = "    " <> k <> " = " <> showNuss v

-- | Stash a nut: @:nuss name wert@
processStash :: Text -> Kobel -> ReplResult
processStash input env =
  case T.words input of
    (name:rest) ->
      let valText = T.unwords rest
          val = parseSimpleValue valText
          env' = stash name val env
      in Display ("  🌰 " <> name <> " = " <> showNuss val) env'
    [] -> Display "  ❌ Syntax: :nuss NAME WERT" env

-- | Crack (evaluate) an expression: @:knack ausdruck@
processCrack :: Text -> Kobel -> ReplResult
processCrack input env =
  let expr = parseSimpleExpr input
      result = crack env expr
  in Display ("  🌰 " <> showNuss result) env

-- | Process a bare expression (variable lookup or simple math).
processExpr :: Text -> Kobel -> ReplResult
processExpr input env =
  let result = crack env (parseSimpleExpr input)
  in Display ("  🌰 " <> showNuss result) env

-- | Parse a simple value from text.
parseSimpleValue :: Text -> Nuss
parseSimpleValue t
  | t == "wahr" || t == "ja" || t == "✅"   = Wahrheit True
  | t == "falsch" || t == "nein" || t == "❌" = Wahrheit False
  | t == "nichts" || t == "∅"               = Nichts
  | isNumber t                               = Zahl (read (T.unpack t))
  | "\"" `T.isPrefixOf` t && "\"" `T.isSuffixOf` t
    = Wort (T.drop 1 (T.dropEnd 1 t))
  | isEmoji t                                = Emoji t
  | otherwise                                = Wort t
  where
    isNumber s = case reads (T.unpack s) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False
    isEmoji s = not (T.null s) && T.head s > '\x1F000'

-- | Parse a simple expression from text.
-- Supports: variable names, numbers, simple binary ops
parseSimpleExpr :: Text -> Baum
parseSimpleExpr input =
  let ws = T.words input
  in case ws of
    []    -> Blatt Nichts
    [w]   -> parseAtom w
    [a, op, b] | op `elem` ["+", "-", "*", "/", "==", "!="]
      -> Ruf op [parseAtom a, parseAtom b]
    (f:args) -> Ruf f (map parseAtom args)

-- | Parse an atom (single token).
parseAtom :: Text -> Baum
parseAtom t
  | isNumber t = Blatt (Zahl (read (T.unpack t)))
  | t == "wahr" || t == "✅"   = Blatt (Wahrheit True)
  | t == "falsch" || t == "❌" = Blatt (Wahrheit False)
  | "\"" `T.isPrefixOf` t     = Blatt (Wort (T.drop 1 (T.dropEnd 1 t)))
  | otherwise                  = Name t
  where
    isNumber s = case reads (T.unpack s) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

-- | Help text.
helpText :: Text
helpText = T.unlines
  [ ""
  , "  🐿️ HASEL Hilfe"
  , ""
  , "  Befehle:"
  , "    :hilfe         Zeige diese Hilfe"
  , "    :universen     Zeige alle FP-Universen unter dem HASEL-Schirm"
  , "    :kobel         Zeige alle gespeicherten Nüsse"
  , "    :nuss x 42     Speichere Nuss x mit Wert 42"
  , "    :knack 1 + 2   Werte einen Ausdruck aus"
  , "    :tschuess      Verlasse den Spielplatz"
  , ""
  , "  Ausdruecke:"
  , "    42             Zahl"
  , "    \"hallo\"        Text"
  , "    wahr / falsch  Wahrheitswert"
  , "    x + y          Rechnung (+ - * /)"
  , "    x == y         Vergleich"
  , "    pi             Vordefinierte Nuss"
  , ""
  , "  🌰 HASEL IT! ☂️"
  ]

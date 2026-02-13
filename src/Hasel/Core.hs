{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

-- | HASEL Core — the nuts and bolts of the squirrel universe.
--
-- Metaphor mapping:
--   Nuss   (🌰) = value / expression
--   Kobel  (🏠) = environment / scope (squirrel nest)
--   Baum   (🌳) = expression tree / AST
--   Wald   (🌲) = program / forest of expressions
--   Eich   (🐿️) = evaluator / the squirrel that cracks nuts
module Hasel.Core
  ( Nuss(..)
  , Kobel
  , Baum(..)
  , Wald
  , emptyKobel
  , stash
  , fetch
  , crack
  , showNuss
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | A Nuss is any value in HASEL — the fundamental unit.
-- Like a nut gathered by a squirrel, it can be many things.
data Nuss
  = Zahl Double          -- ^ 🔢 Number
  | Wort Text            -- ^ 📝 Text/String
  | Emoji Text           -- ^ 😃 Emoji symbol
  | Wahrheit Bool        -- ^ ✅❌ Boolean (truth)
  | Liste [Nuss]         -- ^ 📋 List of nuts
  | Paar (Nuss, Nuss)    -- ^ 🤝 Pair/Tuple
  | Funktion Text Baum   -- ^ λ  Lambda/Function (param name, body)
  | Nichts               -- ^ ∅  Nothing/Unit
  deriving (Show, Eq)

-- | A Kobel is where a squirrel stores its nuts — the environment.
type Kobel = Map Text Nuss

-- | A Baum is an expression tree — what the squirrel evaluates.
data Baum
  = Blatt Nuss           -- ^ 🍂 Leaf: a literal value
  | Name Text            -- ^ 🏷️ Variable reference
  | Ruf Text [Baum]      -- ^ 📢 Function call (name, arguments)
  | Wenn Baum Baum Baum  -- ^ 🔀 If-then-else
  | Binde Text Baum Baum -- ^ 🪢 Let-binding (name, value, body)
  | Kette [Baum]         -- ^ ⛓️ Sequence of expressions
  | Lambda Text Baum     -- ^ λ  Lambda expression
  deriving (Show, Eq)

-- | A Wald (forest) is a complete program — a list of top-level trees.
type Wald = [Baum]

-- | An empty Kobel — a squirrel's nest before winter.
emptyKobel :: Kobel
emptyKobel = Map.empty

-- | Stash a nut in the Kobel (store a value).
stash :: Text -> Nuss -> Kobel -> Kobel
stash = Map.insert

-- | Fetch a nut from the Kobel (look up a value).
fetch :: Text -> Kobel -> Maybe Nuss
fetch = Map.lookup

-- | Crack a nut — evaluate a Baum in a Kobel.
crack :: Kobel -> Baum -> Nuss
crack _   (Blatt n)          = n
crack env (Name x)           = maybe Nichts id (fetch x env)
crack env (Kette bs)         = foldl (\_ b -> crack env b) Nichts bs
crack env (Wenn cond ja nein) =
  case crack env cond of
    Wahrheit True  -> crack env ja
    Wahrheit False -> crack env nein
    _              -> Nichts
crack env (Binde x val body) =
  let v    = crack env val
      env' = stash x v env
  in crack env' body
crack _env (Lambda param body) = Funktion param body
crack env (Ruf name args) =
  let evArgs = map (crack env) args
  in case fetch name env of
       Just (Funktion param body) ->
         case evArgs of
           (a:_) -> crack (stash param a env) body
           []    -> Nichts
       _ -> applyBuiltin name evArgs

-- | Built-in functions — the squirrel's instincts.
applyBuiltin :: Text -> [Nuss] -> Nuss
applyBuiltin "+"   [Zahl a, Zahl b]       = Zahl (a + b)
applyBuiltin "-"   [Zahl a, Zahl b]       = Zahl (a - b)
applyBuiltin "*"   [Zahl a, Zahl b]       = Zahl (a * b)
applyBuiltin "/"   [Zahl a, Zahl b]       = if b /= 0 then Zahl (a / b) else Nichts
applyBuiltin "=="  [a, b]                  = Wahrheit (a == b)
applyBuiltin "!="  [a, b]                  = Wahrheit (a /= b)
applyBuiltin "not" [Wahrheit b]            = Wahrheit (not b)
applyBuiltin "und" [Wahrheit a, Wahrheit b] = Wahrheit (a && b)
applyBuiltin "oder" [Wahrheit a, Wahrheit b] = Wahrheit (a || b)
applyBuiltin "liste" xs                    = Liste xs
applyBuiltin "kopf" [Liste (x:_)]         = x
applyBuiltin "rest" [Liste (_:xs)]         = Liste xs
applyBuiltin "laenge" [Liste xs]           = Zahl (fromIntegral (length xs))
applyBuiltin "laenge" [Wort t]             = Zahl (fromIntegral (T.length t))
applyBuiltin "verbinde" [Wort a, Wort b]  = Wort (a <> b)
applyBuiltin "zeige" [n]                   = Wort (showNuss n)
applyBuiltin _ _                           = Nichts

-- | Display a Nuss as human-readable text.
showNuss :: Nuss -> Text
showNuss (Zahl n)
  | n == fromIntegral (round n) = T.pack (show (round n :: Integer))
  | otherwise                   = T.pack (show n)
showNuss (Wort t)        = t
showNuss (Emoji e)       = e
showNuss (Wahrheit True) = "✅"
showNuss (Wahrheit False) = "❌"
showNuss (Liste ns)      = "[ " <> T.intercalate ", " (map showNuss ns) <> " ]"
showNuss (Paar (a, b))   = "(" <> showNuss a <> ", " <> showNuss b <> ")"
showNuss (Funktion p _)  = "λ " <> p <> " → ..."
showNuss Nichts          = "∅"

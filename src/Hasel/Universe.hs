{-# LANGUAGE OverloadedStrings #-}

-- | HASEL Universe — importing the best of all FP worlds.
--
-- HASEL doesn't compete — it ABSORBS. Like a squirrel gathering
-- every nut in the forest: Haselnuss, Walnuss, Erdnuss, Macadamia.
-- All stored in one Kobel. ☂️
--
-- @
--   import best(Haskell)      -- λ  types, monads, purity
--   import best(PureScript)   -- ⊳  row types, effects
--   import best(Elm)          -- 🌲 simplicity, TEA
--   import best(Idris)        -- 🔬 dependent types
--   import best(Agda)         -- 📐 formal verification
--   wrap(emoji)               -- 😃 universal interface
--   wrap(Spass)               -- 🎮 SPIELEN!!!
-- @
module Hasel.Universe
  ( Universe(..)
  , Paradigm(..)
  , allUniverses
  , paradigmsOf
  , beschreibung
  , showUniverse
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | A functional programming universe that HASEL imports from.
data Universe
  = Haskell     -- ^ λ  The mothership: monads, type classes, laziness
  | PureScript  -- ^ ⊳  Row types, algebraic effects, JS target
  | Elm         -- ^ 🌲 The Elm Architecture, simplicity, no runtime errors
  | Idris       -- ^ 🔬 Dependent types, proofs as programs
  | Agda        -- ^ 📐 Formal verification, cubical type theory
  | Dhall       -- ^ ⚙️ Configuration as code, total functions
  | Nix         -- ^ ❄️ Reproducible builds, pure package management
  deriving (Show, Eq, Enum, Bounded)

-- | What each universe brings to the HASEL umbrella.
data Paradigm
  = Monaden           -- ^ Haskell monads
  | Typenklassen      -- ^ Type classes
  | Faulheit          -- ^ Lazy evaluation
  | Reinheit          -- ^ Purity / referential transparency
  | Zeilentypen       -- ^ Row types (PureScript)
  | Effekte           -- ^ Algebraic effects
  | TEA               -- ^ The Elm Architecture
  | Einfachheit       -- ^ Simplicity / beginner-friendly
  | AbhaengigeTypen   -- ^ Dependent types (Idris)
  | BeweiseAlsCode    -- ^ Proofs as programs
  | Totalitaet        -- ^ Total functions (Dhall)
  | Reproduzierbar    -- ^ Reproducibility (Nix)
  deriving (Show, Eq)

-- | All universes under the HASEL umbrella ☂️
allUniverses :: [Universe]
allUniverses = [minBound .. maxBound]

-- | What paradigms each universe contributes.
paradigmsOf :: Universe -> [Paradigm]
paradigmsOf Haskell    = [Monaden, Typenklassen, Faulheit, Reinheit]
paradigmsOf PureScript = [Zeilentypen, Effekte, Typenklassen, Reinheit]
paradigmsOf Elm        = [TEA, Einfachheit, Reinheit]
paradigmsOf Idris      = [AbhaengigeTypen, BeweiseAlsCode, Totalitaet]
paradigmsOf Agda       = [AbhaengigeTypen, BeweiseAlsCode, Totalitaet]
paradigmsOf Dhall      = [Totalitaet, Reinheit]
paradigmsOf Nix        = [Reproduzierbar, Reinheit]

-- | Emoji symbol for each universe.
showUniverse :: Universe -> Text
showUniverse Haskell    = "λ  Haskell"
showUniverse PureScript = "⊳  PureScript"
showUniverse Elm        = "🌲 Elm"
showUniverse Idris      = "🔬 Idris"
showUniverse Agda       = "📐 Agda"
showUniverse Dhall      = "⚙️  Dhall"
showUniverse Nix        = "❄️  Nix"

-- | Describe a universe in HASEL terms.
beschreibung :: Universe -> Text
beschreibung Haskell    = "Die Mutter aller Nüsse — Monaden, Typenklassen, Faulheit"
beschreibung PureScript = "Zeilentypen und Effekte — die JS-Nuss"
beschreibung Elm        = "Einfachheit über alles — keine Laufzeitfehler"
beschreibung Idris      = "Abhängige Typen — Beweise als Programme"
beschreibung Agda       = "Formale Verifikation — kubische Typentheorie"
beschreibung Dhall      = "Konfiguration als Code — totale Funktionen"
beschreibung Nix        = "Reproduzierbare Builds — reines Paketmanagement"

{-# LANGUAGE OverloadedStrings #-}

-- | HASEL — Haskell + Emoji Language ☂️
--
-- The first language where the compiler smiles. 😃🌰
--
-- HASEL imports the best of all functional programming universes
-- and wraps them in an emoji-powered interface accessible to
-- squirrels of all ages.
--
-- @
--   import best(Haskell)      -- λ  monads, type classes, purity
--   import best(PureScript)   -- ⊳  row types, algebraic effects
--   import best(Elm)          -- 🌲 TEA, simplicity
--   import best(Idris)        -- 🔬 dependent types, proofs
--   import best(Agda)         -- 📐 formal verification
--   wrap(emoji)               -- 😃 universal interface
--   wrap(Spass)               -- 🎮 SPIELEN!!!
-- @
--
-- Metaphor:
--   Hasel   = the bush (Corylus) where nuts grow  = the LANGUAGE
--   Nuss    = the nut                              = a VALUE
--   Kobel   = the squirrel's nest                  = the ENVIRONMENT
--   Baum    = the tree                             = an EXPRESSION
--   Wald    = the forest                           = a PROGRAM
--   Eich    = the squirrel (Eichhörnchen)          = the EVALUATOR
--
-- emoji go! 🐿️🚀
module Hasel
  ( -- * Re-exports
    module Hasel.Core
  , module Hasel.Emoji
  , module Hasel.Universe
  , module Hasel.Repl
  ) where

import Hasel.Core
import Hasel.Emoji
import Hasel.Universe
import Hasel.Repl

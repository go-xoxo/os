{-# LANGUAGE OverloadedStrings #-}

-- | HASEL Emoji — the universal interface.
--
-- Emoji operators make HASEL accessible to squirrels of all ages.
-- No English required. No CS degree required. Just emoji go! 🐿️🚀
module Hasel.Emoji
  ( -- * Emoji constructors (build nuts from emoji)
    nuss
  , wort
  , emoji
  , ja
  , nein
  , nichts
    -- * Emoji operators (combine nuts)
  , (🌰)
  , (⚡)
  , (💚)
  , (🔗)
  , (🌊)
    -- * Emoji display
  , anzeigen
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hasel.Core

-- | 🌰 Create a numeric Nuss.
nuss :: Double -> Nuss
nuss = Zahl

-- | 📝 Create a text Nuss.
wort :: Text -> Nuss
wort = Wort

-- | 😃 Create an emoji Nuss.
emoji :: Text -> Nuss
emoji = Emoji

-- | ✅ True.
ja :: Nuss
ja = Wahrheit True

-- | ❌ False.
nein :: Nuss
nein = Wahrheit False

-- | ∅ Nothing.
nichts :: Nuss
nichts = Nichts

-- | 🌰 Stash operator — put a named nut into a Kobel.
--   @name 🌰 value@ stores the value.
(🌰) :: Text -> Nuss -> (Text, Nuss)
name 🌰 value = (name, value)
infixr 5 🌰

-- | ⚡ Apply/bind operator — apply a function to a value.
(⚡) :: Nuss -> Nuss -> Nuss
(Funktion param body) ⚡ arg =
  crack (stash param arg emptyKobel) body
_ ⚡ _ = Nichts
infixl 4 ⚡

-- | 💚 Pure/return — wrap any nut in purity.
(💚) :: a -> a
(💚) = id
infixr 0 💚

-- | 🔗 Chain — combine two nuts into a pair.
(🔗) :: Nuss -> Nuss -> Nuss
a 🔗 b = Paar (a, b)
infixr 3 🔗

-- | 🌊 Sequence — collect nuts into a list.
(🌊) :: Nuss -> Nuss -> Nuss
a 🌊 b = case b of
  Liste xs -> Liste (a : xs)
  _        -> Liste [a, b]
infixr 2 🌊

-- | Display a Nuss to the terminal with emoji flair.
anzeigen :: Nuss -> IO ()
anzeigen n = TIO.putStrLn $ "🌰 " <> showNuss n

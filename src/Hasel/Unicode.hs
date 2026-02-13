{-# LANGUAGE OverloadedStrings #-}

-- | Unicode 18 mappings for HASEL icons.
--
-- Unicode 18.0 (released 2025) added 11,328 characters including
-- CJK Unified Ideographs Extension J, 9 new emoji, and the
-- UAE Dirham sign (U+20CE). HASEL maps its world icons to
-- Private Use Area codepoints in the Supplementary PUA-A block
-- (U+F0000–U+FFFFF) for custom SVG-backed glyphs, while also
-- providing mappings to standard Unicode 18 symbols for fallback
-- rendering in environments without the HASEL font.
--
-- The pipeline:
--   1. Assign each concept a PUA codepoint (for HASEL font)
--   2. Map to nearest standard Unicode 18 symbol (for fallback)
--   3. Generate SVG for each PUA glyph
--   4. Typst show rules intercept the PUA char and render the SVG
module Hasel.Unicode
  ( -- * Codepoint allocation
    UnicodeMapping (..)
  , allMappings
  , worldMappings
  , featureMappings

    -- * Unicode 18 standard symbols used as fallbacks
  , unicode18NewSymbols

    -- * Lookup
  , svgForCodepoint
  , fallbackFor
  ) where

import Data.Char (chr, ord)
import Data.Text (Text)
import qualified Data.Text as T

-- | A mapping between a HASEL concept and its Unicode representation.
data UnicodeMapping = UnicodeMapping
  { umName :: Text
  -- ^ Human-readable name
  , umPuaCodepoint :: Char
  -- ^ Private Use Area codepoint (HASEL font glyph)
  , umFallbackCodepoint :: Char
  -- ^ Standard Unicode 18 fallback symbol
  , umFallbackName :: Text
  -- ^ Official Unicode name of the fallback character
  , umSvgPath :: Text
  -- ^ Path to SVG icon file
  , umCategory :: MappingCategory
  }

data MappingCategory = WorldIcon | FeatureIcon | OperatorIcon
  deriving (Show, Eq)

-- | All HASEL Unicode mappings.
allMappings :: [UnicodeMapping]
allMappings = worldMappings ++ featureMappings

-- | World-level icon mappings.
-- Each world gets a PUA codepoint with a standard Unicode fallback.
worldMappings :: [UnicodeMapping]
worldMappings =
  [ UnicodeMapping "Haskell"    '\x1CC00' '\x03BB' "GREEK SMALL LETTER LAMBDA"    "assets/icons/haskell.svg"    WorldIcon
  , UnicodeMapping "PureScript" '\x1CC10' '\x25B3' "WHITE UP-POINTING TRIANGLE"   "assets/icons/purescript.svg" WorldIcon
  , UnicodeMapping "Elm"        '\x1CC20' '\x1F333' "DECIDUOUS TREE"              "assets/icons/elm.svg"        WorldIcon
  , UnicodeMapping "Idris"      '\x1CC30' '\x1F409' "DRAGON"                      "assets/icons/idris.svg"      WorldIcon
  , UnicodeMapping "Agda"       '\x1CC40' '\x2200' "FOR ALL"                      "assets/icons/agda.svg"       WorldIcon
  , UnicodeMapping "Dhall"      '\x1CC50' '\x2699' "GEAR"                         "assets/icons/dhall.svg"      WorldIcon
  , UnicodeMapping "Nix"        '\x1CC60' '\x2744' "SNOWFLAKE"                    "assets/icons/nix.svg"        WorldIcon
  , UnicodeMapping "Typst"      '\x1CC70' '\x1D54B' "MATHEMATICAL DOUBLE-STRUCK CAPITAL T" "assets/icons/typst.svg" WorldIcon
  ]

-- | Feature-level icon mappings.
-- Key FP concepts get their own symbols.
featureMappings :: [UnicodeMapping]
featureMappings =
  [ -- Type system concepts
    UnicodeMapping "Monad"           '\x1CC01' '\x226B' "MUCH GREATER-THAN (>>=)" "assets/icons/monad.svg"      FeatureIcon
  , UnicodeMapping "Functor"         '\x1CC02' '\x2919' "LEFTWARDS ARROW TAIL"    "assets/icons/functor.svg"    FeatureIcon
  , UnicodeMapping "Type Class"      '\x1CC03' '\x1D4C8' "MATHEMATICAL SCRIPT SMALL S" "assets/icons/typeclass.svg" FeatureIcon
  , UnicodeMapping "Dependent Type"  '\x1CC04' '\x03A0' "GREEK CAPITAL LETTER PI" "assets/icons/deptype.svg"    FeatureIcon
  , UnicodeMapping "Row Type"        '\x1CC05' '\x2982' "Z NOTATION TYPE COLON"   "assets/icons/rowtype.svg"    FeatureIcon

    -- Effect concepts
  , UnicodeMapping "Pure"            '\x1CC06' '\x2605' "BLACK STAR"              "assets/icons/pure.svg"       FeatureIcon
  , UnicodeMapping "Effect"          '\x1CC07' '\x26A1' "HIGH VOLTAGE"            "assets/icons/effect.svg"     FeatureIcon
  , UnicodeMapping "IO"              '\x1CC08' '\x21C4' "RIGHTWARDS ARROW OVER LEFTWARDS ARROW" "assets/icons/io.svg" FeatureIcon

    -- Operators
  , UnicodeMapping "Compose"         '\x1CC80' '\x2218' "RING OPERATOR"           "assets/icons/compose.svg"    OperatorIcon
  , UnicodeMapping "Apply"           '\x1CC81' '\x229B' "CIRCLED ASTERISK"        "assets/icons/apply.svg"      OperatorIcon
  , UnicodeMapping "Bind"            '\x1CC82' '\x226B' "MUCH GREATER-THAN"       "assets/icons/bind.svg"       OperatorIcon
  , UnicodeMapping "Map"             '\x1CC83' '\x21A6' "RIGHTWARDS ARROW FROM BAR" "assets/icons/map.svg"      OperatorIcon
  ]

-- | Notable new symbols in Unicode 18.0 relevant to HASEL.
-- These are standard codepoints, not PUA.
unicode18NewSymbols :: [(Char, Text, Text)]
unicode18NewSymbols =
  [ ('\x20CE', "UAE DIRHAM SIGN", "New currency symbol in Unicode 18.0")
  , ('\x1CC00', "OUTLINED LATIN CAPITAL LETTER A", "Latin Extended-G block — new in Unicode 18.0, range U+1CC00-U+1CCCF")
  , ('\x31350', "CJK UNIFIED IDEOGRAPH-31350", "CJK Extension J — 11,328 new ideographs")
  -- Emoji 18.0
  , ('\x1FACE', "FINGERPRINT", "New emoji 18.0")
  , ('\x1FAD3', "BEANS", "New emoji 18.0")
  , ('\x1FA7C', "SHOVEL", "New emoji 18.0")
  ]

-- | Look up the SVG path for a given PUA codepoint.
svgForCodepoint :: Char -> Maybe Text
svgForCodepoint c =
  case filter (\m -> umPuaCodepoint m == c) allMappings of
    (m : _) -> Just (umSvgPath m)
    [] -> Nothing

-- | Get the standard Unicode fallback for a PUA codepoint.
fallbackFor :: Char -> Maybe (Char, Text)
fallbackFor c =
  case filter (\m -> umPuaCodepoint m == c) allMappings of
    (m : _) -> Just (umFallbackCodepoint m, umFallbackName m)
    [] -> Nothing

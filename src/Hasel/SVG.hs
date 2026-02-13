{-# LANGUAGE OverloadedStrings #-}

-- | SVG icon generation for HASEL world symbols.
--
-- Each FP world in HASEL gets a distinctive SVG icon that can be:
--   1. Embedded in an OpenType-SVG font at its PUA codepoint
--   2. Referenced by Typst show rules for inline rendering
--   3. Used standalone in documentation and UI
--
-- The four OpenType color table formats:
--   - COLR/CPAL: Vector layers with palettes (best support, smallest size)
--   - SVG:       Full SVG embedded per glyph (richest, but large)
--   - CBDT/CBLC: Bitmap at fixed sizes (Google's original Noto Emoji approach)
--   - sbix:      Apple's bitmap format (used in Apple Color Emoji)
--
-- HASEL uses COLR/CPAL for the font and standalone SVG for Typst.
module Hasel.SVG
  ( -- * SVG generation
    generateWorldIcon
  , generateFeatureIcon
  , generateAllIcons
  , svgDocument

    -- * SVG primitives
  , SvgElement (..)
  , svgToText
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | A minimal SVG element representation.
data SvgElement
  = SvgCircle Double Double Double Text  -- cx cy r fill
  | SvgRect Double Double Double Double Text  -- x y w h fill
  | SvgPath Text Text  -- d fill
  | SvgText Double Double Text Text Text  -- x y fontFamily fontSize content
  | SvgGroup [SvgElement]
  deriving (Show)

-- | Render an SVG element to text.
svgToText :: SvgElement -> Text
svgToText (SvgCircle cx cy r fill) =
  "<circle cx=\"" <> showT cx <> "\" cy=\"" <> showT cy
    <> "\" r=\"" <> showT r <> "\" fill=\"" <> fill <> "\"/>"
svgToText (SvgRect x y w h fill) =
  "<rect x=\"" <> showT x <> "\" y=\"" <> showT y
    <> "\" width=\"" <> showT w <> "\" height=\"" <> showT h
    <> "\" fill=\"" <> fill <> "\"/>"
svgToText (SvgPath d fill) =
  "<path d=\"" <> d <> "\" fill=\"" <> fill <> "\"/>"
svgToText (SvgText x y family size content) =
  "<text x=\"" <> showT x <> "\" y=\"" <> showT y
    <> "\" font-family=\"" <> family <> "\" font-size=\"" <> size
    <> "\">" <> content <> "</text>"
svgToText (SvgGroup elems) =
  "<g>" <> T.concat (map svgToText elems) <> "</g>"

showT :: Double -> Text
showT = T.pack . show

-- | Wrap elements in a complete SVG document.
svgDocument :: Double -> Double -> [SvgElement] -> Text
svgDocument w h elems =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    <> "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 "
    <> showT w <> " " <> showT h <> "\" width=\""
    <> showT w <> "\" height=\"" <> showT h <> "\">\n"
    <> T.unlines (map (\e -> "  " <> svgToText e) elems)
    <> "</svg>\n"

-- | Generate an SVG icon for a HASEL world.
generateWorldIcon :: Text -> Text
generateWorldIcon "Haskell" = svgDocument 100 100
  [ -- Lambda symbol in purple
    SvgRect 0 0 100 100 "#4B275F"
  , SvgPath "M 10 90 L 35 50 L 10 10 L 30 10 L 50 42 L 70 10 L 90 10 L 62 50 L 90 90 L 70 90 L 50 58 L 30 90 Z" "#F5F5F5"
  ]
generateWorldIcon "PureScript" = svgDocument 100 100
  [ -- Triangle in dark grey
    SvgRect 0 0 100 100 "#1D222D"
  , SvgPath "M 50 10 L 90 85 L 10 85 Z" "#F5F5F5"
  , SvgPath "M 50 28 L 76 75 L 24 75 Z" "#1D222D"
  ]
generateWorldIcon "Elm" = svgDocument 100 100
  [ -- Tangram pieces
    SvgRect 0 0 100 100 "#1293D8"
  , SvgPath "M 10 10 L 50 50 L 10 90 Z" "#F0AD00"
  , SvgPath "M 50 50 L 90 10 L 90 50 Z" "#60B5CC"
  , SvgPath "M 50 50 L 90 50 L 90 90 L 50 90 Z" "#5A6378"
  , SvgPath "M 10 10 L 50 10 L 30 30 Z" "#7FD13B"
  ]
generateWorldIcon "Idris" = svgDocument 100 100
  [ -- Dragon stylized
    SvgRect 0 0 100 100 "#B30000"
  , SvgPath "M 50 10 L 80 40 L 70 50 L 90 90 L 50 70 L 10 90 L 30 50 L 20 40 Z" "#F5F5F5"
  ]
generateWorldIcon "Agda" = svgDocument 100 100
  [ -- Pentagon with forall symbol
    SvgRect 0 0 100 100 "#2E5090"
  , SvgPath "M 50 10 L 88 37 L 74 82 L 26 82 L 12 37 Z" "#F5F5F5"
  , SvgText 35 65 "Noto Sans" "36" "\x2200"
  ]
generateWorldIcon "Dhall" = svgDocument 100 100
  [ -- Flower/gear
    SvgRect 0 0 100 100 "#FFC107"
  , SvgCircle 50 50 25 "#4E342E"
  , SvgCircle 50 50 15 "#FFC107"
  , SvgPath "M 50 10 L 55 25 L 45 25 Z" "#4E342E"
  , SvgPath "M 50 90 L 55 75 L 45 75 Z" "#4E342E"
  , SvgPath "M 10 50 L 25 55 L 25 45 Z" "#4E342E"
  , SvgPath "M 90 50 L 75 55 L 75 45 Z" "#4E342E"
  ]
generateWorldIcon "Nix" = svgDocument 100 100
  [ -- Snowflake
    SvgRect 0 0 100 100 "#5277C3"
  , SvgPath "M 50 10 L 53 45 L 85 27 L 56 50 L 85 73 L 53 55 L 50 90 L 47 55 L 15 73 L 44 50 L 15 27 L 47 45 Z" "#F5F5F5"
  ]
generateWorldIcon "Typst" = svgDocument 100 100
  [ -- T in a circle
    SvgRect 0 0 100 100 "#239DAD"
  , SvgCircle 50 50 40 "#F5F5F5"
  , SvgRect 30 25 40 10 "#239DAD"
  , SvgRect 45 25 10 50 "#239DAD"
  ]
generateWorldIcon name = svgDocument 100 100
  [ -- Default: question mark
    SvgRect 0 0 100 100 "#9E9E9E"
  , SvgText 30 70 "Noto Sans" "60" "?"
  ]

-- | Generate an SVG icon for an FP feature/concept.
generateFeatureIcon :: Text -> Text
generateFeatureIcon "Monad" = svgDocument 100 100
  [ SvgRect 0 0 100 100 "#7B1FA2"
  , SvgText 10 70 "Noto Sans Mono" "48" ">>="
  ]
generateFeatureIcon "Functor" = svgDocument 100 100
  [ SvgRect 0 0 100 100 "#0288D1"
  , SvgText 15 70 "Noto Sans Mono" "48" "<$>"
  ]
generateFeatureIcon "Pure" = svgDocument 100 100
  [ SvgRect 0 0 100 100 "#2E7D32"
  , SvgPath "M 50 10 L 61 40 L 93 40 L 67 58 L 77 88 L 50 70 L 23 88 L 33 58 L 7 40 L 39 40 Z" "#F5F5F5"
  ]
generateFeatureIcon "Effect" = svgDocument 100 100
  [ SvgRect 0 0 100 100 "#F57F17"
  , SvgPath "M 55 10 L 35 48 L 55 48 L 45 90 L 75 45 L 55 45 Z" "#F5F5F5"
  ]
generateFeatureIcon name = svgDocument 100 100
  [ SvgRect 0 0 100 100 "#607D8B"
  , SvgText 15 65 "Noto Sans" "14" name
  ]

-- | Generate all SVG icons and return them as (filename, content) pairs.
generateAllIcons :: [(Text, Text)]
generateAllIcons =
  [ ("assets/icons/haskell.svg",    generateWorldIcon "Haskell")
  , ("assets/icons/purescript.svg", generateWorldIcon "PureScript")
  , ("assets/icons/elm.svg",        generateWorldIcon "Elm")
  , ("assets/icons/idris.svg",      generateWorldIcon "Idris")
  , ("assets/icons/agda.svg",       generateWorldIcon "Agda")
  , ("assets/icons/dhall.svg",      generateWorldIcon "Dhall")
  , ("assets/icons/nix.svg",        generateWorldIcon "Nix")
  , ("assets/icons/typst.svg",      generateWorldIcon "Typst")
  , ("assets/icons/monad.svg",      generateFeatureIcon "Monad")
  , ("assets/icons/functor.svg",    generateFeatureIcon "Functor")
  , ("assets/icons/pure.svg",       generateFeatureIcon "Pure")
  , ("assets/icons/effect.svg",     generateFeatureIcon "Effect")
  ]

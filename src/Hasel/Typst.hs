{-# LANGUAGE OverloadedStrings #-}

-- | Typst document generation for HASEL.
--
-- Generates Typst (.typ) source that renders HASEL documentation
-- using:
--   - Noto Sans / Noto Serif / Noto Sans Mono for text
--   - Noto Color Emoji for emoji fallback
--   - Show rules mapping PUA codepoints to SVG icons
--   - Unicode 18 symbols for inline math and operators
--
-- The Noto font stack:
--   Noto Sans          — UI text, headings
--   Noto Serif         — body text, long-form
--   Noto Sans Mono     — code, operators
--   Noto Sans Symbols  — mathematical symbols
--   Noto Sans Symbols2 — additional symbols
--   Noto Sans Math     — mathematical typesetting
--   Noto Color Emoji   — emoji rendering (CBDT/SVG tables)
module Hasel.Typst
  ( -- * Document generation
    generateTypstDocument
  , generateShowRules
  , generateFontConfig
  , generateWorldSection

    -- * Typst primitives
  , TypstBlock (..)
  , renderBlock
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (ord)

import Hasel.Unicode (UnicodeMapping (..), worldMappings, featureMappings)

-- | A block of Typst content.
data TypstBlock
  = TypstRaw Text
  -- ^ Raw Typst source
  | TypstHeading Int Text
  -- ^ Heading with level (1-4) and text
  | TypstParagraph Text
  -- ^ A paragraph of text
  | TypstCode Text Text
  -- ^ Code block with language and content
  | TypstTable [[Text]]
  -- ^ Table with rows of cells
  | TypstShowRule Text Text
  -- ^ Show rule: pattern → replacement
  deriving (Show)

-- | Render a TypstBlock to Typst source text.
renderBlock :: TypstBlock -> Text
renderBlock (TypstRaw t) = t
renderBlock (TypstHeading level title) =
  T.replicate level "=" <> " " <> title <> "\n"
renderBlock (TypstParagraph text) = text <> "\n\n"
renderBlock (TypstCode lang content) =
  "```" <> lang <> "\n" <> content <> "\n```\n"
renderBlock (TypstTable rows) =
  let ncols = case rows of
        (r:_) -> length r
        [] -> 0
      header = "#table(\n  columns: " <> T.pack (show ncols) <> ",\n"
      cells = T.intercalate ",\n" $
        concatMap (\row -> map (\cell -> "  [" <> cell <> "]") row) rows
  in header <> cells <> "\n)\n"
renderBlock (TypstShowRule pattern replacement) =
  "#show \"" <> pattern <> "\": " <> replacement <> "\n"

-- | Generate the Noto font configuration block.
generateFontConfig :: Text
generateFontConfig = T.unlines
  [ "// ═══════════════════════════════════════"
  , "// NOTO FONT STACK"
  , "// ═══════════════════════════════════════"
  , "// Primary text font: Noto Sans for UI clarity"
  , "#set text(font: (\"Noto Sans\", \"Noto Sans Symbols\", \"Noto Sans Symbols 2\", \"Noto Color Emoji\"), size: 11pt)"
  , ""
  , "// Code blocks: Noto Sans Mono"
  , "#show raw: set text(font: \"Noto Sans Mono\", size: 10pt)"
  , ""
  , "// Math: Noto Sans Math"
  , "#show math.equation: set text(font: \"Noto Sans Math\")"
  , ""
  , "// Headings: Noto Serif for contrast"
  , "#show heading: set text(font: \"Noto Serif\", weight: \"bold\")"
  , ""
  , "// Page setup"
  , "#set page(margin: 2cm)"
  , "#set par(justify: true, leading: 0.8em)"
  ]

-- | Generate Typst show rules that map PUA codepoints to SVG icons.
-- This is the bridge: Unicode character → SVG image inline.
generateShowRules :: Text
generateShowRules =
  let worldRules = map mkShowRule worldMappings
      featureRules = map mkShowRule featureMappings
  in T.unlines $
       [ "// ═══════════════════════════════════════"
       , "// SVG-IN-UNICODE SHOW RULES"
       , "// ═══════════════════════════════════════"
       , "// Each PUA codepoint is intercepted and replaced"
       , "// with its SVG icon rendered at text height."
       , "// This implements the SVG-in-Unicode pipeline:"
       , "//   Unicode 18 codepoint → Typst show rule → SVG → PDF glyph"
       , ""
       , "// --- World icons ---"
       ] ++ worldRules ++
       [ ""
       , "// --- Feature icons ---"
       ] ++ featureRules

mkShowRule :: UnicodeMapping -> Text
mkShowRule um =
  "#show \""
    <> escapeTypstChar (umPuaCodepoint um)
    <> "\": image(\""
    <> umSvgPath um
    <> "\", height: 1em)  // "
    <> umName um
    <> " (U+"
    <> hexCodepoint (umPuaCodepoint um)
    <> " → fallback: "
    <> umFallbackName um
    <> ")"

escapeTypstChar :: Char -> Text
escapeTypstChar c = "\\u{" <> hexCodepoint c <> "}"

hexCodepoint :: Char -> Text
hexCodepoint c =
  let hex = T.pack $ showHex (ord c) ""
  in T.toUpper hex
  where
    showHex :: Int -> String -> String
    showHex n acc
      | n < 16 = hexDigit n : acc
      | otherwise = showHex (n `div` 16) (hexDigit (n `mod` 16) : acc)
    hexDigit d
      | d < 10 = toEnum (fromEnum '0' + d)
      | otherwise = toEnum (fromEnum 'A' + d - 10)

-- | Generate a Typst section documenting one HASEL world.
generateWorldSection :: Text -> Text -> Text -> [(Text, Text)] -> [Text] -> Text
generateWorldSection name emoji tagline features insights =
  T.unlines $
    [ "== " <> emoji <> " " <> name
    , ""
    , "_" <> tagline <> "_"
    , ""
    , "=== Key Features"
    , ""
    ] ++ map (\(n, d) -> "- *" <> n <> "*: " <> d) features
    ++ [ ""
       , "=== Insights"
       , ""
       ] ++ map (\i -> "#block(fill: luma(245), inset: 12pt, radius: 4pt)[" <> i <> "]") insights
    ++ [""]

-- | Generate a complete Typst document showcasing all HASEL worlds.
generateTypstDocument :: Text
generateTypstDocument = T.unlines
  [ "// ═══════════════════════════════════════════════════════"
  , "// HASEL — Haskell + Emoji Language"
  , "// SVG Icons × Unicode 18 × Typst × All Noto Fonts"
  , "// Generated by Squirrel OS"
  , "// ═══════════════════════════════════════════════════════"
  , ""
  , generateFontConfig
  , ""
  , generateShowRules
  , ""
  , "// ═══════════════════════════════════════"
  , "// DOCUMENT"
  , "// ═══════════════════════════════════════"
  , ""
  , "= HASEL: Functional Programming United"
  , ""
  , "#align(center)["
  , "  #text(size: 24pt, font: \"Noto Color Emoji\")[🐿️☂️⚡]"
  , "]"
  , ""
  , "#v(1em)"
  , ""
  , "#align(center)["
  , "  _Eight worlds. One umbrella. Emoji go._"
  , "]"
  , ""
  , "#v(2em)"
  , ""
  , "== What is HASEL?"
  , ""
  , "HASEL imports the best ideas from every functional programming ecosystem"
  , "and unifies them under a single, emoji-friendly interface. Each \"world\""
  , "represents a language with unique strengths:"
  , ""
  , "#table("
  , "  columns: 4,"
  , "  [*World*], [*Icon*], [*Best Feature*], [*Tagline*],"
  , "  [Haskell],    [λ], [Type Classes],    [_Purity + laziness + type classes_],"
  , "  [PureScript], [△], [Row Types],       [_Strict Haskell for the browser_],"
  , "  [Elm],        [🌳], [No Exceptions],   [_FP for everyone_],"
  , "  [Idris],      [🐉], [Dependent Types], [_Proving programs correct_],"
  , "  [Agda],       [∀],  [Unicode Syntax],  [_Mathematics as code_],"
  , "  [Dhall],      [⚙️], [Totality],        [_Configuration as a language_],"
  , "  [Nix],        [❄️], [Reproducibility], [_The functional OS_],"
  , "  [Typst],      [𝕋],  [Show Rules],      [_Modern typesetting_],"
  , ")"
  , ""
  , "#pagebreak()"
  , ""
  , worldSectionHaskell
  , "#pagebreak()"
  , worldSectionPureScript
  , "#pagebreak()"
  , worldSectionElm
  , "#pagebreak()"
  , worldSectionIdris
  , "#pagebreak()"
  , worldSectionAgda
  , "#pagebreak()"
  , worldSectionDhall
  , "#pagebreak()"
  , worldSectionNix
  , "#pagebreak()"
  , worldSectionTypst
  , ""
  , "// ═══════════════════════════════════════"
  , "// UNICODE 18 REFERENCE"
  , "// ═══════════════════════════════════════"
  , ""
  , "#pagebreak()"
  , ""
  , "= Appendix: Unicode 18.0 & SVG Integration"
  , ""
  , "Unicode 18.0 (2025) added 11,328 characters including CJK Extension J,"
  , "the UAE Dirham sign (U+20CE), and 9 new emoji. HASEL maps its icons"
  , "to Private Use Area codepoints with standard Unicode 18 fallbacks."
  , ""
  , "== OpenType Color Table Formats"
  , ""
  , "#table("
  , "  columns: 3,"
  , "  [*Format*], [*Type*], [*Used By*],"
  , "  [COLR/CPAL], [Vector layers + palettes], [Noto Color Emoji v2, Windows],"
  , "  [SVG],       [Full SVG per glyph],       [Firefox, Adobe fonts],"
  , "  [CBDT/CBLC], [Bitmap at fixed sizes],    [Noto Color Emoji v1, Android],"
  , "  [sbix],      [Apple bitmap format],       [Apple Color Emoji],"
  , ")"
  , ""
  , "== The SVG-in-Unicode Pipeline"
  , ""
  , "```"
  , "Design SVG icon"
  , "  → Assign PUA codepoint (U+1CC00-U+1CC7F)"
  , "  → Map fallback to standard Unicode 18 symbol"
  , "  → Typst show rule intercepts codepoint"
  , "  → SVG rendered inline at 1em height"
  , "  → PDF output with embedded vector glyph"
  , "```"
  , ""
  , "== Noto Font Coverage"
  , ""
  , "#table("
  , "  columns: 3,"
  , "  [*Font*], [*Covers*], [*Use in HASEL*],"
  , "  [Noto Sans],          [Latin, Cyrillic, Greek, ...], [UI text, headings],"
  , "  [Noto Serif],         [Same scripts, serif style],   [Body text],"
  , "  [Noto Sans Mono],     [Monospaced variant],          [Code, operators],"
  , "  [Noto Sans Math],     [Mathematical symbols],        [Equations],"
  , "  [Noto Sans Symbols],  [Miscellaneous symbols],       [FP notation],"
  , "  [Noto Sans Symbols 2],[Extended symbols],             [Additional notation],"
  , "  [Noto Color Emoji],   [Full emoji set],              [World emoji, fallback],"
  , ")"
  , ""
  , "// ═══════════════════════════════════════"
  , "// END"
  , "// ═══════════════════════════════════════"
  , "#align(center)[#text(size: 18pt, font: \"Noto Color Emoji\")[🐿️🌰]]"
  , "#align(center)[_HASEL IT_]"
  ]

------------------------------------------------------------------------
-- Per-world sections with real content
------------------------------------------------------------------------

worldSectionHaskell :: Text
worldSectionHaskell = generateWorldSection "Haskell" "🧪" "Pure functions, lazy evaluation, type classes — the research language that became production-ready."
  [ ("Type Classes", "Ad-hoc polymorphism via type class dispatch. Inspired Rust traits, Scala implicits, Swift protocols.")
  , ("Lazy Evaluation", "Non-strict by default. Thunks evaluated on demand. Enables infinite structures and compositional streaming.")
  , ("Monads", "Sequencing with context. IO, State, Reader, Writer. Key insight: monads are about COMPOSITION, not side effects.")
  , ("GHC Extensions", "100+ extensions from RankNTypes to TypeFamilies. GHC = compiler + research laboratory.")
  , ("Purity", "No side effects in functions. All effects tracked in types. Equational reasoning: substitute equals for equals.")
  ]
  [ "The learning curve is real but misunderstood. The difficulty isn't monads — it's unlearning imperative habits."
  , "Haskell-the-report and Haskell-via-GHC are different languages. In practice, OverloadedStrings, GADTs, TypeFamilies are mandatory."
  , "Cabal v3 (Nix-style builds) resolved the Cabal-vs-Stack split. Use cabal for new projects."
  , "Laziness enables beautiful abstractions but creates space leaks. BangPatterns and seq are essential in production."
  ]

worldSectionPureScript :: Text
worldSectionPureScript = generateWorldSection "PureScript" "🌐" "Haskell's ideas, strict evaluation, row types — compiling to JavaScript and beyond."
  [ ("Row Types", "Extensible records with row polymorphism. Functions require specific fields without knowing full record shape.")
  , ("Effect System", "Algebraic effects via the Effect monad. Each effect is a row type label. Compose effects freely.")
  , ("JavaScript FFI", "Clean FFI to JS. Import any JS function with a type annotation. Compiler verifies PureScript-side types.")
  , ("Strict Evaluation", "Unlike Haskell, strict by default. No space leaks from lazy thunks. Predictable performance.")
  , ("Halogen", "Type-safe UI framework. Parent-child communication via queries and slots. More powerful than Elm for large apps.")
  ]
  [ "Started as 'Haskell for JS' but evolved its own identity. Strict evaluation and row types make it distinct."
  , "Tiny community compared to Haskell/Elm, but higher average package quality on Pursuit."
  , "Backends for Erlang (purerl) and Go exist. The compiler architecture enables polyglot FP."
  , "Adopted Haskell's best ideas while fixing pain points: strict by default, no String-vs-Text confusion, better records."
  ]

worldSectionElm :: Text
worldSectionElm = generateWorldSection "Elm" "🌳" "No runtime exceptions, friendly errors, The Elm Architecture — FP for everyone."
  [ ("The Elm Architecture", "Model-Update-View. Every app follows the same pattern. TEA inspired Redux, Vuex, SwiftUI.")
  , ("No Runtime Exceptions", "Elm programs do not crash. The compiler catches all errors. Maybe/Result replace null/exceptions.")
  , ("Compiler Error Messages", "The gold standard. Explains what went wrong, why, and suggests fixes. Rust and Scala 3 followed Elm's lead.")
  , ("Semantic Versioning Enforcement", "Package manager detects API changes and enforces semver. The TOOL decides the version.")
  , ("Managed Effects", "All effects via Cmd/Sub. No escape hatch. The runtime owns effects; your code is pure.")
  ]
  [ "The restrictions (no type classes, no FFI escape, single maintainer) are features for TEAM reliability."
  , "TEA changed frontend forever. Redux = TEA for React. Vuex = TEA for Vue. SwiftUI = TEA."
  , "Maintained by one person. Coherent design but existential risk. Releases are infrequent."
  , "The easiest typed FP language. No monads, no type classes, no category theory. Just functions, records, unions."
  ]

worldSectionIdris :: Text
worldSectionIdris = generateWorldSection "Idris" "🐉" "Dependent types, totality, quantitative types — proving programs correct."
  [ ("Dependent Types", "Types depend on values. Vector n carries its length. The compiler PROVES bounds safety.")
  , ("Totality Checking", "Compiler verifies functions terminate and cover all inputs. Total functions = proofs.")
  , ("Elaborator Reflection", "Metaprogramming via compiler reflection. Write Idris that generates Idris, type-checked.")
  , ("Quantitative Type Theory", "Idris 2 uses QTT: bindings carry quantities (0, 1, ω). Enables linear types — use once.")
  , ("Interactive Editing", "Type-driven dev in editor. Compiler suggests case splits, generates bodies from types.")
  ]
  [ "Proved dependent types can be practical — general-purpose language first, proof assistant second."
  , "Idris 2 is a rewrite in Idris 1, bootstrapped. Chez Scheme backend replaced custom C codegen."
  , "Dependent types let you prove properties, but you PAY in code complexity. Choose what to prove carefully."
  , "Fewer libraries than Python has web frameworks. But every program explores what types can express."
  ]

worldSectionAgda :: Text
worldSectionAgda = generateWorldSection "Agda" "🔬" "Unicode syntax, cubical types, formal proofs — mathematics as executable code."
  [ ("Unicode Syntax", "Native Unicode identifiers: ∀, →, ≡, ∷. Mathematical notation becomes executable code.")
  , ("Dependent Pattern Matching", "Splitting on a constructor refines types of other arguments. The compiler tracks refinement.")
  , ("Cubical Type Theory", "Paths between types are interval functions. Univalence computes. HoTT with computation.")
  , ("Mixfix Operators", "if_then_else_ is a valid name. Underscores mark argument positions. DSLs read like prose.")
  , ("Proof Irrelevance", "Mark proofs irrelevant with @0. Compiler erases them at runtime. Zero-cost proofs.")
  ]
  [ "Where programming meets mathematics. Used to formalize homotopy type theory and category theory."
  , "A proof assistant that happens to be a programming language. No web framework, no database driver."
  , "Unicode isn't nice-to-have — it's how you write code. Every file looks like a math paper."
  , "The standard library reads like a graduate algebra textbook. Data.Nat proves its own algebraic laws."
  ]

worldSectionDhall :: Text
worldSectionDhall = generateWorldSection "Dhall" "⚙️" "Total, typed, importable — configuration as a real language."
  [ ("Total Language", "Every expression terminates. No infinite loops. Config evaluation always completes.")
  , ("Type-Safe Configuration", "Config files with real types. Import, validate, compose with full type checking.")
  , ("Built-in Import System", "Import from URLs, files, env vars. SHA256 content-addressed for integrity.")
  , ("Normalization", "Unique normal form per expression. Semantic equality, not textual equality.")
  , ("Multi-Format Output", "Compile to JSON, YAML, XML. Write once, output in any format.")
  ]
  [ "Proved configuration needs types. After Dhall, YAML feels like assembly."
  , "Totality makes URL imports safe — code CANNOT loop forever. Only language where untrusted code is safe by design."
  , "Technically superior to YAML, but adoption requires convincing whole teams."
  , "Functions in config: one Dhall function generates dev, staging, prod configs. No copy-paste-modify."
  ]

worldSectionNix :: Text
worldSectionNix = generateWorldSection "Nix" "❄️" "Reproducible builds, immutable packages, NixOS — the functional operating system."
  [ ("Reproducible Builds", "Same derivation → same output, byte for byte. No 'works on my machine'.")
  , ("Nix Store", "/nix/store with cryptographic hashes. Multiple versions coexist without conflict.")
  , ("Flakes", "Hermetic project definitions. flake.nix declares inputs/outputs. Lock file pins everything.")
  , ("NixOS", "Entire OS as a functional expression. Configuration is Nix. Rollback to any generation.")
  , ("Development Shells", "nix develop = reproducible dev environment. Identical tools for every team member.")
  ]
  [ "Steepest learning curve in all FP tooling. Underdocumented, cryptic errors. But nothing else compares."
  , "Nixpkgs has 100,000+ packages. Larger than Debian, AUR, or Homebrew."
  , "Flakes are 'experimental' after years of use. Old channels and new flakes coexist awkwardly."
  , "Packages as values, builds as pure functions, store as cache. HASEL thinking applied to DevOps."
  ]

worldSectionTypst :: Text
worldSectionTypst = generateWorldSection "Typst" "📝" "Show rules, incremental compilation, content as values — modern typesetting."
  [ ("Show Rules", "Intercept and transform elements. #show \"X\": image(\"x.svg\") replaces text with SVG. Pattern matching on content.")
  , ("Incremental Compilation", "Only recompiles what changed. Sub-second feedback for large documents.")
  , ("Scripting Language", "Real language: variables, functions, loops. Not LaTeX macros — actual programming.")
  , ("Content as Values", "Document content is first-class. Store paragraphs in variables, pass headings to functions.")
  , ("Font Discovery", "Finds system fonts automatically. #set text(font: \"Noto Sans\") — all Noto variants work.")
  ]
  [ "What LaTeX would be if designed today. Modern syntax, fast compilation, sane errors."
  , "Show rules = pattern matching on document structure. FP thinking in typesetting."
  , "Show rules enable mapping Unicode to SVG inline. The bridge: codepoint → show rule → SVG → PDF."
  , "Pre-1.0, breaking changes happen. But pace is extraordinary and community is responsive."
  ]

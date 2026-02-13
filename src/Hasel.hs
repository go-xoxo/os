{-# LANGUAGE OverloadedStrings #-}

-- | HASEL — Haskell + Emoji Language
--
-- A unified umbrella over functional programming paradigms.
-- Each "world" represents a language ecosystem, exposing its
-- best ideas through a common interface. HASEL imports the
-- best of all FP universes and wraps them in emoji for
-- Eichhörnchen of every age.
--
-- The name: Hasel = the hazelnut bush (Corylus).
-- The nuts are the code. The bush is the language.
-- The forest is the filesystem. Scatter-hoard and grow.
module Hasel
  ( -- * Core types
    World (..)
  , Feature (..)
  , Insight (..)
  , HaselIcon (..)
  , InsightCategory (..)

    -- * World registry
  , allWorlds
  , worldByName
  , featuresOf

    -- * The umbrella
  , haselIt
  , bestOf
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | A functional programming world absorbed into HASEL.
data World = World
  { worldName :: Text
  -- ^ Canonical name (e.g. "Haskell", "PureScript")
  , worldEmoji :: Text
  -- ^ Representative emoji sequence
  , worldIcon :: HaselIcon
  -- ^ SVG icon mapped to a Unicode 18 codepoint
  , worldFeatures :: [Feature]
  -- ^ Best ideas imported from this world
  , worldInsights :: [Insight]
  -- ^ Real observations about the ecosystem
  , worldTagline :: Text
  -- ^ One-line essence
  }

-- | A feature imported from a world into HASEL.
data Feature = Feature
  { featureName :: Text
  , featureDescription :: Text
  , featureOriginWorld :: Text
  -- ^ Which world pioneered this
  , featureUnicodeSymbol :: Char
  -- ^ Unicode 18 symbol representing this feature
  }

-- | A real insight about a world — not marketing, but truth.
data Insight = Insight
  { insightTitle :: Text
  , insightBody :: Text
  , insightCategory :: InsightCategory
  }

data InsightCategory
  = TypeSystem
  | Effects
  | Tooling
  | Ecosystem
  | Performance
  | Learning
  | Interop
  deriving (Show, Eq)

-- | An icon mapped to a Unicode 18 codepoint, backed by an SVG.
data HaselIcon = HaselIcon
  { iconCodepoint :: Char
  -- ^ The Unicode 18 codepoint this icon occupies
  , iconSvgPath :: Text
  -- ^ Relative path to the SVG file
  , iconDescription :: Text
  -- ^ Accessible description
  }

-- | All worlds in the HASEL umbrella.
allWorlds :: [World]
allWorlds =
  [ haskellWorld
  , pureScriptWorld
  , elmWorld
  , idrisWorld
  , agdaWorld
  , dhallWorld
  , nixWorld
  , typstWorld
  ]

-- | Look up a world by name (case-insensitive).
worldByName :: Text -> Maybe World
worldByName name =
  case filter (\w -> T.toLower (worldName w) == T.toLower name) allWorlds of
    (w : _) -> Just w
    [] -> Nothing

-- | Extract all features from a world.
featuresOf :: World -> [(Text, Text)]
featuresOf w = map (\f -> (featureName f, featureDescription f)) (worldFeatures w)

-- | HASEL IT: given a problem domain, find the best world.
-- Returns all worlds sorted by relevance to the domain keywords.
haselIt :: Text -> [World]
haselIt query =
  let keywords = T.words (T.toLower query)
      score w =
        length
          [ ()
          | kw <- keywords
          , feat <- worldFeatures w
          , kw `T.isInfixOf` T.toLower (featureDescription feat)
              || kw `T.isInfixOf` T.toLower (featureName feat)
          ]
   in sortByScore score allWorlds
  where
    sortByScore f = map snd . reverse . sortPairs . map (\w -> (f w, w))
    sortPairs [] = []
    sortPairs (x : xs) =
      let smaller = [p | p <- xs, fst p <= fst x]
          bigger = [p | p <- xs, fst p > fst x]
       in sortPairs smaller ++ [x] ++ sortPairs bigger

-- | Extract the single best feature from each world.
bestOf :: [World] -> [(Text, Feature)]
bestOf = concatMap (\w -> case worldFeatures w of
  (f : _) -> [(worldName w, f)]
  [] -> [])

------------------------------------------------------------------------
-- World definitions (the real content)
------------------------------------------------------------------------

haskellWorld :: World
haskellWorld = World
  { worldName = "Haskell"
  , worldEmoji = "\x1F9EA" -- 🧪 test tube = purity
  , worldIcon = HaselIcon '\x1CC00' "assets/icons/haskell.svg" "Haskell lambda"
  , worldFeatures =
      [ Feature "Type Classes" "Ad-hoc polymorphism via type class dispatch. The original mechanism that inspired Rust traits, Scala implicits, and Swift protocols." "Haskell" '\x1CC01'
      , Feature "Lazy Evaluation" "Non-strict semantics by default. Thunks are only evaluated when their value is demanded. Enables infinite data structures and compositional streaming." "Haskell" '\x1CC02'
      , Feature "Monads" "Sequencing computations with context. IO, State, Reader, Writer — each monad captures a pattern of composition. The key insight: monads are about composition, not side effects." "Haskell" '\x1CC03'
      , Feature "GHC Extensions" "Over 100 language extensions from RankNTypes to TypeFamilies. GHC is both a compiler and a research laboratory for type theory." "Haskell" '\x1CC04'
      , Feature "Purity" "Functions have no side effects. All effects are tracked in the type system. This makes equational reasoning possible: you can substitute equals for equals." "Haskell" '\x1CC05'
      ]
  , worldInsights =
      [ Insight "The Learning Wall" "Haskell's learning curve is real but misunderstood. The difficulty isn't monads — it's unlearning imperative habits. Once you think in transformations instead of instructions, everything clicks." Learning
      , Insight "GHC Is the Language" "Haskell-the-report and Haskell-as-used-via-GHC are different languages. In practice, extensions like OverloadedStrings, GADTs, and TypeFamilies are mandatory. The report is a historical document." TypeSystem
      , Insight "The Ecosystem Split" "Cabal vs Stack split the community for years. cabal-install v3 (Nix-style builds) largely resolved this, but old tutorials still confuse newcomers. Use cabal for new projects." Tooling
      , Insight "Lazy Evaluation Tradeoffs" "Laziness enables beautiful abstractions but creates space leaks. Profiling Haskell memory usage requires understanding thunk accumulation. Strictness annotations (BangPatterns, seq) are essential in production." Performance
      ]
  , worldTagline = "Pure functions, lazy evaluation, type classes — the research language that became production-ready."
  }

pureScriptWorld :: World
pureScriptWorld = World
  { worldName = "PureScript"
  , worldEmoji = "\x1F310" -- 🌐 globe = web target
  , worldIcon = HaselIcon '\x1CC10' "assets/icons/purescript.svg" "PureScript triangle"
  , worldFeatures =
      [ Feature "Row Types" "Extensible records with row polymorphism. Functions can require specific fields without knowing the full record shape. Solves the expression problem for records." "PureScript" '\x1CC11'
      , Feature "Effect System" "Algebraic effects tracked in the type system via the Effect monad. Each effect (Console, Random, HTTP) is a row type label. Compose effects freely." "PureScript" '\x1CC12'
      , Feature "JavaScript FFI" "Clean foreign function interface to JavaScript. Import any JS function with a type annotation. The compiler verifies Haskell-side types; JS-side is your responsibility." "PureScript" '\x1CC13'
      , Feature "Strict Evaluation" "Unlike Haskell, PureScript is strict by default. No space leaks from lazy thunks. Performance characteristics are predictable and match JavaScript semantics." "PureScript" '\x1CC14'
      , Feature "Halogen" "A type-safe UI framework with a component model. Parent-child communication via queries and slots. More complex than Elm but more powerful for large apps." "PureScript" '\x1CC15'
      ]
  , worldInsights =
      [ Insight "Haskell Without the Laziness" "PureScript started as 'Haskell that compiles to JS' but evolved its own identity. Strict evaluation, row types, and the effect system make it a distinct language, not a port." TypeSystem
      , Insight "Small but Dedicated Community" "The PureScript community is tiny compared to Haskell or Elm. This means fewer libraries but higher average quality. Every package on Pursuit is likely maintained by someone who cares." Ecosystem
      , Insight "The Backend Story" "PureScript-to-JS was the original target, but backends for Erlang (purerl) and Go exist. The compiler architecture allows multiple backends, making PureScript a polyglot FP language." Interop
      , Insight "Learning from Haskell's Mistakes" "PureScript adopted Haskell's best ideas (type classes, ADTs, do-notation) while fixing pain points (strict by default, no String-vs-Text confusion, better records)." Learning
      ]
  , worldTagline = "Haskell's ideas, strict evaluation, row types — compiling to JavaScript and beyond."
  }

elmWorld :: World
elmWorld = World
  { worldName = "Elm"
  , worldEmoji = "\x1F333" -- 🌳 tree = Elm tree
  , worldIcon = HaselIcon '\x1CC20' "assets/icons/elm.svg" "Elm tangram"
  , worldFeatures =
      [ Feature "The Elm Architecture" "Model-Update-View. Every Elm app follows the same pattern: a model (state), an update function (state machine), and a view function (render). TEA inspired Redux, Vuex, and SwiftUI." "Elm" '\x1CC21'
      , Feature "No Runtime Exceptions" "Elm programs do not crash in production. The compiler catches all possible errors at build time. Maybe and Result replace null and exceptions." "Elm" '\x1CC22'
      , Feature "Compiler Error Messages" "The gold standard for error messages. The compiler doesn't just say what went wrong — it explains why and suggests fixes. Other languages (Rust, Scala 3) explicitly followed Elm's lead." "Elm" '\x1CC23'
      , Feature "Semantic Versioning Enforcement" "The Elm package manager automatically detects API changes and enforces semantic versioning. You cannot publish a breaking change as a patch. The tool decides the version, not the human." "Elm" '\x1CC24'
      , Feature "Managed Effects" "All side effects go through Cmd and Sub. No escape hatch, no unsafe operations. The runtime owns all effects; your code is pure functions over messages." "Elm" '\x1CC25'
      ]
  , worldInsights =
      [ Insight "Walled Garden by Design" "Elm's restrictions (no type classes, no FFI escape hatch, single maintainer) are features, not bugs. The goal is reliability for teams, not expressiveness for individuals. This creates real tension." Ecosystem
      , Insight "TEA Changed Frontend Forever" "Even if you never write Elm, you use TEA. Redux is TEA for React. Vuex is TEA for Vue. SwiftUI's state management is TEA. Elm's biggest impact was on other ecosystems." TypeSystem
      , Insight "The Bus Factor" "Elm is maintained primarily by one person (Evan Czaplicki). This ensures coherent design but creates existential risk. Releases are infrequent and the community has fragmented as a result." Ecosystem
      , Insight "Perfect for Beginners" "Elm is the easiest typed FP language to learn. No monads, no type classes, no category theory. Just functions, records, and union types. If someone asks 'where do I start with FP?', the answer is Elm." Learning
      ]
  , worldTagline = "No runtime exceptions, friendly errors, The Elm Architecture — FP for everyone."
  }

idrisWorld :: World
idrisWorld = World
  { worldName = "Idris"
  , worldEmoji = "\x1F409" -- 🐉 dragon = Idris the dragon
  , worldIcon = HaselIcon '\x1CC30' "assets/icons/idris.svg" "Idris dragon"
  , worldFeatures =
      [ Feature "Dependent Types" "Types can depend on values. A Vector (n : Nat) carries its length in the type. The compiler proves your list operations can never go out of bounds." "Idris" '\x1CC31'
      , Feature "Totality Checking" "The compiler can verify that functions terminate and cover all inputs. Total functions are proofs. Partial functions are explicitly marked." "Idris" '\x1CC32'
      , Feature "Elaborator Reflection" "Metaprogramming by reflecting the compiler's elaboration process. Write Idris code that generates Idris code, with full access to the type checker." "Idris" '\x1CC33'
      , Feature "Quantitative Type Theory" "Idris 2 uses QTT: each variable binding carries a quantity (0, 1, or unrestricted). This enables linear types — resources that must be used exactly once." "Idris" '\x1CC34'
      , Feature "Interactive Editing" "Type-driven development in the editor. The compiler suggests case splits, generates function bodies from types, and fills holes. The type IS the specification." "Idris" '\x1CC35'
      ]
  , worldInsights =
      [ Insight "Dependent Types for Mortals" "Idris proved that dependent types can be practical, not just academic. Unlike Coq or Agda, Idris is designed as a general-purpose programming language first, proof assistant second." TypeSystem
      , Insight "Idris 2 is a Rewrite" "Idris 2 is not an update — it's a new language written in Idris 1, then bootstrapped. The Chez Scheme backend replaced custom C codegen. Performance improved dramatically." Performance
      , Insight "The Proof Burden" "Dependent types let you prove properties, but you PAY for those proofs in code complexity. Simple functions become complex when you thread proofs through them. Choose what to prove carefully." TypeSystem
      , Insight "Small Ecosystem, Big Ideas" "Idris has fewer libraries than Python has web frameworks. But every Idris program is an exploration of what types can express. The ecosystem is a research lab, not a package registry." Ecosystem
      ]
  , worldTagline = "Dependent types, totality, quantitative types — proving programs correct."
  }

agdaWorld :: World
agdaWorld = World
  { worldName = "Agda"
  , worldEmoji = "\x1F52C" -- 🔬 microscope = formal verification
  , worldIcon = HaselIcon '\x1CC40' "assets/icons/agda.svg" "Agda pentagon"
  , worldFeatures =
      [ Feature "Unicode Syntax" "Agda natively supports Unicode identifiers and operators. You write ∀, →, ≡, ∷ directly. Mathematical notation becomes executable code." "Agda" '\x1CC41'
      , Feature "Pattern Matching on Types" "Dependent pattern matching where splitting on a constructor refines the types of other arguments. The compiler knows that if you matched Suc n, the length decreased." "Agda" '\x1CC42'
      , Feature "Cubical Type Theory" "Cubical Agda implements homotopy type theory with computational content. Paths between types are functions from the interval. Univalence computes." "Agda" '\x1CC43'
      , Feature "Mixfix Operators" "Define operators with holes: if_then_else_ is a valid function name. Parsing follows the underscores. This enables DSLs that read like natural language." "Agda" '\x1CC44'
      , Feature "Proof Irrelevance" "Mark proofs as irrelevant with a dot pattern or @0 quantity. The compiler erases them at runtime. Proofs guide type checking but cost nothing in execution." "Agda" '\x1CC45'
      ]
  , worldInsights =
      [ Insight "The Mathematician's Language" "Agda is where programming meets mathematics. It's used to formalize proofs in homotopy type theory, category theory, and algebra. If you can state it as a type, Agda can verify it." TypeSystem
      , Insight "Not for Production" "Agda is a proof assistant that happens to be a programming language. You CAN write programs in it, but the ecosystem assumes you're proving theorems. No web framework, no database driver." Ecosystem
      , Insight "Unicode is Not Optional" "In Agda, Unicode isn't a nice-to-have — it's how you write code. Every Agda file looks like a math paper. This is powerful for mathematicians and alien to most programmers." Tooling
      , Insight "The Standard Library is a Textbook" "Agda's standard library reads like a graduate algebra textbook. Every definition comes with proofs of its properties. Data.Nat doesn't just define natural numbers — it proves their algebraic laws." Learning
      ]
  , worldTagline = "Unicode syntax, cubical types, formal proofs — mathematics as executable code."
  }

dhallWorld :: World
dhallWorld = World
  { worldName = "Dhall"
  , worldEmoji = "\x2699\xFE0F" -- ⚙️ gear = configuration
  , worldIcon = HaselIcon '\x1CC50' "assets/icons/dhall.svg" "Dhall flower"
  , worldFeatures =
      [ Feature "Total Language" "Dhall is total: every expression terminates. No infinite loops, no recursion. This guarantees that configuration evaluation always completes." "Dhall" '\x1CC51'
      , Feature "Type-Safe Configuration" "Configuration files with a real type system. Import, validate, and compose configs with full type checking. No more YAML typos in production." "Dhall" '\x1CC52'
      , Feature "Built-in Import System" "Import expressions from URLs, files, or environment variables. Imports are content-addressed with SHA256 hashes for integrity. Reproducible configuration." "Dhall" '\x1CC53'
      , Feature "Normalization" "Every Dhall expression has a unique normal form. Two configs that produce the same result have the same normal form. Semantic equality, not textual equality." "Dhall" '\x1CC54'
      , Feature "Multi-Format Output" "Dhall compiles to JSON, YAML, XML, and more. Write your config once in Dhall, output it in whatever format your tool expects." "Dhall" '\x1CC55'
      ]
  , worldInsights =
      [ Insight "Configuration is Code" "Dhall proved that configuration languages need types. After using Dhall, going back to YAML feels like going from Haskell to assembly. The type errors alone justify the switch." TypeSystem
      , Insight "Totality Enables Fearless Imports" "Because Dhall is total, importing code from a URL is safe — it CANNOT loop forever or crash. This is the only language where running untrusted code is actually safe by design." Effects
      , Insight "The Adoption Challenge" "Dhall is technically superior to YAML/JSON for configuration, but adoption requires convincing entire teams. Most teams choose 'good enough' (YAML + linter) over 'correct' (Dhall)." Ecosystem
      , Insight "Functions in Configuration" "Dhall lets you abstract configuration with functions and generics. A single Dhall function can generate configs for dev, staging, and prod. No more copy-paste-modify." Tooling
      ]
  , worldTagline = "Total, typed, importable — configuration as a real language."
  }

nixWorld :: World
nixWorld = World
  { worldName = "Nix"
  , worldEmoji = "\x2744\xFE0F" -- ❄️ snowflake = reproducibility
  , worldIcon = HaselIcon '\x1CC60' "assets/icons/nix.svg" "Nix snowflake"
  , worldFeatures =
      [ Feature "Reproducible Builds" "Every build is determined by its inputs. The same derivation always produces the same output, byte for byte. No 'works on my machine'." "Nix" '\x1CC61'
      , Feature "Nix Store" "All packages live in /nix/store, identified by cryptographic hashes of their complete dependency tree. Multiple versions coexist without conflict." "Nix" '\x1CC62'
      , Feature "Flakes" "Hermetic, composable project definitions. A flake.nix declares inputs (dependencies) and outputs (packages, dev shells, NixOS configs). The lock file pins everything." "Nix" '\x1CC63'
      , Feature "NixOS" "An entire operating system as a functional expression. Your OS configuration is a Nix expression. Rollback to any previous generation instantly." "Nix" '\x1CC64'
      , Feature "Development Shells" "nix develop gives you a reproducible development environment. Every team member gets identical tools, libraries, and compilers. Works on Linux and macOS." "Nix" '\x1CC65'
      ]
  , worldInsights =
      [ Insight "The Learning Cliff" "Nix has the steepest learning curve in all of FP tooling. The language is underdocumented, error messages are cryptic, and the manual assumes you already understand derivations. But once it clicks, nothing else compares." Learning
      , Insight "Nixpkgs is Massive" "Nixpkgs is the largest package repository in existence — over 100,000 packages. More than Debian, AUR, or Homebrew. Maintained by a community of thousands." Ecosystem
      , Insight "The Flakes Controversy" "Flakes are still 'experimental' after years of widespread use. The old nix-channel system and the new flakes system coexist awkwardly. The community is split on governance." Tooling
      , Insight "Functional Package Management" "Nix treats packages like values in a functional language. Packages are immutable, builds are pure functions, and the store is a cache of evaluated expressions. This is HASEL thinking applied to DevOps." TypeSystem
      ]
  , worldTagline = "Reproducible builds, immutable packages, NixOS — the functional operating system."
  }

typstWorld :: World
typstWorld = World
  { worldName = "Typst"
  , worldEmoji = "\x1F4DD" -- 📝 memo = typesetting
  , worldIcon = HaselIcon '\x1CC70' "assets/icons/typst.svg" "Typst T"
  , worldFeatures =
      [ Feature "Show Rules" "Intercept and transform any element during rendering. #show \"X\": image(\"x.svg\") replaces text with SVG. Pattern matching on document content." "Typst" '\x1CC71'
      , Feature "Incremental Compilation" "Typst recompiles only what changed. Edit a paragraph, only that paragraph is re-typeset. Sub-second feedback even for large documents." "Typst" '\x1CC72'
      , Feature "Scripting Language" "Typst has a built-in scripting language with variables, functions, loops, and conditionals. It's not LaTeX macros — it's a real programming language embedded in a typesetting system." "Typst" '\x1CC73'
      , Feature "Content as Values" "In Typst, document content is a first-class value. You can store paragraphs in variables, pass headings to functions, and compose documents programmatically." "Typst" '\x1CC74'
      , Feature "Font Discovery" "Typst finds system fonts automatically and supports embedded project fonts. Set font family with #set text(font: \"Noto Sans\") — all Noto variants work." "Typst" '\x1CC75'
      ]
  , worldInsights =
      [ Insight "LaTeX's Successor" "Typst is what LaTeX would be if designed today. Modern syntax, fast compilation, sane error messages. It won't replace LaTeX for legacy documents, but new projects should consider Typst first." Tooling
      , Insight "Show Rules are Pattern Matching" "Typst's show rules are function pattern matching on document structure. #show heading: it => ... is the same idea as a Haskell case expression over a sum type. FP thinking in typesetting." TypeSystem
      , Insight "The SVG-in-Unicode Bridge" "Typst's show rules enable mapping Unicode codepoints to SVG images inline. Combined with Noto fonts for text, this creates a pipeline: Unicode 18 symbol → show rule → SVG render → PDF glyph." Interop
      , Insight "Young but Moving Fast" "Typst is pre-1.0 and breaking changes happen. But the pace of improvement is extraordinary. The community is small, enthusiastic, and the core team is responsive." Ecosystem
      ]
  , worldTagline = "Show rules, incremental compilation, content as values — modern typesetting."
  }

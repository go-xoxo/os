# Squirrel OS

## HASEL — Haskell + Emoji Language

All FP universes. One umbrella. Spass fuer Eichhoernchen jeden Alters.

```
  🐿️ ╔═══════════════════════════════════════════╗
  🌰 ║                                           ║
  🌳 ║   H A S E L                                ║
  ☂️  ║   Haskell + Emoji Language                 ║
  ⚡ ║                                           ║
  💚 ║   All FP universes. One umbrella.          ║
  🎮 ║   Spass fuer Eichhoernchen jeden Alters.   ║
  🌰 ║                                           ║
  🐿️ ╚═══════════════════════════════════════════╝
```

### What is HASEL?

HASEL imports the best of all functional programming universes and wraps them in an emoji-powered interface:

| Universe   | Symbol | Imports                                    |
|------------|--------|--------------------------------------------|
| Haskell    | λ      | Monads, Type Classes, Lazy Evaluation      |
| PureScript | ⊳      | Row Types, Algebraic Effects               |
| Elm        | 🌲     | TEA Architecture, Simplicity               |
| Idris      | 🔬     | Dependent Types, Proofs as Programs        |
| Agda       | 📐     | Formal Verification                        |
| Dhall      | ⚙️      | Configuration as Code, Total Functions     |
| Nix        | ❄️      | Reproducible Builds                        |

### The Metaphor

```
Hasel  (the bush)      = the LANGUAGE
Nuss   (the nut)  🌰   = a VALUE
Kobel  (the nest) 🏠   = the ENVIRONMENT
Baum   (the tree) 🌳   = an EXPRESSION
Wald   (the forest)    = a PROGRAM
Eich   (the squirrel) 🐿️ = the EVALUATOR
```

### Getting Started

#### Prerequisites

- [GHC](https://www.haskell.org/ghc/) (>= 8.10)
- [Cabal](https://www.haskell.org/cabal/) (>= 3.0)

#### Build and Run

```bash
cabal build
cabal run squirrel-os
```

#### emoji go!

```
🐿️ > hallo
  🌰 Hallo, Eichhörnchen! 🐿️

🐿️ > :nuss x 7
  🌰 x = 7

🐿️ > x + 3
  🌰 10

🐿️ > :kobel
  🏠 Dein Kobel:
    antwort = 42
    hallo = Hallo, Eichhörnchen! 🐿️
    pi = 3
    x = 7

🐿️ > :universen
  ☂️  HASEL importiert das Beste aus allen Welten:
    λ  Haskell — Monaden, Typenklassen, Faulheit
    ⊳  PureScript — Zeilentypen und Effekte
    🌲 Elm — Einfachheit über alles
    🔬 Idris — Abhängige Typen
    📐 Agda — Formale Verifikation

🐿️ > :tschuess
🐿️ Tschüss! Bis bald im Wald! 🌳🌰
```

### Project Structure

```
squirrel-os/
├── app/
│   └── Main.hs              # emoji go! entry point
├── src/
│   ├── Hasel.hs             # Main HASEL module (re-exports all)
│   ├── Hasel/
│   │   ├── Core.hs          # Core types: Nuss, Kobel, Baum, Wald
│   │   ├── Emoji.hs         # Emoji operators: 🌰 ⚡ 💚 🔗 🌊
│   │   ├── Universe.hs      # FP universe imports under one ☂️
│   │   └── Repl.hs          # Interactive REPL
│   └── LLM/
│       └── OpenAI.hs        # OpenAI API client module
├── squirrel-os.cabal         # Project configuration
└── fourmolu.yaml             # Code formatter settings
```

### HASEL IT!

The first language that doesn't compete — it absorbs. Like a squirrel gathering every nut in the forest.

## License

MIT

# Squirrel OS

A Haskell-based project exploring LLM integration with OpenAI's API.

## Overview

Squirrel OS is an experimental project that provides a Haskell interface for communicating with OpenAI's chat completion API. It demonstrates how to build HTTP clients in Haskell using `http-client-tls` and `aeson` for JSON serialization.

## Getting Started

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) (>= 8.10)
- [Cabal](https://www.haskell.org/cabal/) (>= 3.0)

### Build and Run

```bash
cabal build
cabal run squirrel-os
```

## Project Structure

```
squirrel-os/
├── app/
│   └── Main.hs          # Application entry point
├── src/
│   └── LLM/
│       └── OpenAI.hs    # OpenAI API client module
├── mcp-server/          # 🐿️ HASEL Master MCP Server
│   ├── src/             # TypeScript source files
│   ├── dist/            # Compiled JavaScript
│   └── README.md        # MCP server documentation
├── squirrel-os.cabal    # Project configuration
└── fourmolu.yaml        # Code formatter settings
```

## HASEL Master MCP Server

This repository now includes a powerful Model Context Protocol (MCP) server that provides:

- 🌐 **Web Operations**: Fetch, scrape, and crawl web content
- 🪐 **IPFS**: Distributed content storage and retrieval
- 🎭 **Browser Automation**: Playwright and Puppeteer integration
- 🏠 **DynDNS**: Dynamic DNS updates (DuckDNS, No-IP, etc.)
- 📋 **Clipboard**: Persistent clipboard history and search
- 🔺 **NussKette**: Composable pipeline execution
- 🌰 **HASEL Monads**: Type-safe functional data transformations

See [mcp-server/README.md](mcp-server/README.md) for complete documentation.

## Related Projects

- [A Tour of Đ (Edh)](https://github.com/complyue/tour) -- An interactive tour of the Edh programming language, which runs interpreted atop Haskell/GHC. Edh focuses on reactive events integration with dynamic effect tracking, and its documentation is organized as navigable code snippets in IDE environments.

## License

MIT

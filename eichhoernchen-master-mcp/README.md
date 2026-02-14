# 🐿️ Eichhörnchen Master MCP Server

**The Ultimate MCP Server — HASEL Monads meets Web, IPFS, and Beyond!**

## Overview

This is the master Model Context Protocol (MCP) server for Squirrel OS, implementing the HASEL monad philosophy with comprehensive web, IPFS, browser automation, and DNS capabilities.

## Features

### 🌐 Web Capabilities
- **HTTP Fetch**: Make HTTP requests with full control (GET, POST, PUT, DELETE)
- **Web Scraping**: Extract content using CSS selectors with Cheerio, Playwright, or Puppeteer
- **Web Crawling**: Recursive crawling with depth and pattern controls

### 🪐 IPFS Integration
- **Add Content**: Store content on IPFS with optional pinning
- **Retrieve Content**: Fetch content by CID
- **Gateway Management**: List and use multiple IPFS gateways

### 🎭 Browser Automation
- **Playwright & Puppeteer**: Full browser control
- **Actions**: Navigate, click, type, screenshot, evaluate JS, wait, select, scroll
- **Headless or Headed**: Your choice!

### 🏠 DynDNS Support
- **Update DNS**: Auto-update your dynamic DNS (DuckDNS, NoIP, Cloudflare, etc.)
- **Resolve Records**: Query A, AAAA, CNAME, MX, TXT, NS records

### 📋 Clipboard Management
- **Write**: Store content in clipboard buffer
- **Read**: Read recent clipboard history
- **Search**: Search through clipboard history

### 🔺 NussKette Pipelines
- **Chained Actions**: Create multi-step pipelines combining any tools
- **Monad Types**: Text, Binary, Mixed, URL, IPFS, DOM, DNS, Clipboard, IO

## HASEL Monad Philosophy

Every operation flows through monadic types:

- **TextMonad**: Pure text transformations
- **BinaryMonad**: Binary data (images, PDFs, etc.)
- **MixedMonad**: Combined text + binary
- **URLMonad**: Web resources
- **IPFSMonad**: Distributed content
- **DOMMonad**: Browser DOM manipulation
- **DNSMonad**: DNS resolution and updates
- **ClipboardMonad**: Clipboard operations
- **IOMonad**: General I/O operations

## Installation

```bash
cd eichhoernchen-master-mcp
npm install
npm run build
```

## Configuration

Add to your `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "eichhoernchen-master": {
      "command": "node",
      "args": ["/home/user/os/eichhoernchen-master-mcp/dist/index.js"],
      "env": {
        "IPFS_GATEWAY": "https://ipfs.io",
        "DYNDNS_PROVIDER": "duckdns",
        "DYNDNS_TOKEN": "your-token-here"
      }
    }
  }
}
```

## Tools

### Web Tools
- `web_fetch` — HTTP requests
- `web_scrape` — Extract web content
- `web_crawl` — Crawl websites

### IPFS Tools
- `ipfs_add` — Add content to IPFS
- `ipfs_get` — Retrieve content by CID
- `ipfs_list_gateways` — List available gateways

### Browser Tools
- `browser_session` — Automate browser with actions

### DNS Tools
- `dyndns_update` — Update dynamic DNS
- `dyndns_resolve` — Resolve DNS records

### Clipboard Tools
- `clipboard_write` — Write to clipboard
- `clipboard_read` — Read clipboard history
- `clipboard_search` — Search clipboard

### Pipeline Tools
- `nusskette_run` — Execute multi-step pipelines
- `system_status` — Check server status

## Examples

### Example 1: Scrape and Store on IPFS

```json
{
  "name": "scrape-to-ipfs",
  "steps": [
    {
      "monad": "url",
      "action": "web_scrape",
      "params": {
        "url": "https://example.com",
        "selector": "article"
      }
    },
    {
      "monad": "ipfs",
      "action": "ipfs_add",
      "params": {
        "pin": true
      }
    }
  ]
}
```

### Example 2: Update DynDNS and Browse

```json
{
  "name": "dyndns-browse",
  "steps": [
    {
      "monad": "dns",
      "action": "dyndns_update",
      "params": {
        "hostname": "squirrel.duckdns.org"
      }
    },
    {
      "monad": "dom",
      "action": "browser_session",
      "params": {
        "url": "https://squirrel.duckdns.org",
        "actions": [{"type": "screenshot"}]
      }
    }
  ]
}
```

## License

MIT

---

**Built with 🌰 by the Squirrel OS Team**

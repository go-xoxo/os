# 🐿️ HASEL Master MCP Server

A comprehensive Model Context Protocol (MCP) server that integrates multiple powerful capabilities:

- 🌐 **Web Operations**: Fetch, scrape, and crawl web content
- 🪐 **IPFS**: Distributed content storage and retrieval
- 🎭 **Browser Automation**: Playwright and Puppeteer integration
- 🏠 **DynDNS**: Dynamic DNS updates (DuckDNS, No-IP, etc.)
- 📋 **Clipboard**: Persistent clipboard history and search
- 🔺 **NussKette**: Composable pipeline execution
- 🌰 **HASEL Monads**: Type-safe functional data transformations

## What is HASEL?

**HASEL = Haskell + Squirrel + Monad**

HASEL is a functional programming paradigm that brings type safety and composability to data operations. Every piece of data (a "Nuss" 🌰) flows through a Monad, ensuring predictable transformations and error handling.

### Monad Types

- `text`: Pure text data (strings)
- `binary`: Binary data (buffers, images)
- `mixed`: Mixed content (JSON with embedded binary)
- `url`: Web resources
- `ipfs`: IPFS content
- `dom`: DOM/HTML structures
- `dns`: DNS records
- `clipboard`: Clipboard data
- `io`: I/O operations
- `error`: Error state

## Installation

### Prerequisites

- Node.js >= 18.0.0
- npm or yarn

### Install Dependencies

```bash
cd mcp-server
npm install
```

### Build

```bash
npm run build
```

## Configuration

### Claude Desktop

Add this server to your Claude Desktop configuration:

**macOS/Linux**: `~/Library/Application Support/Claude/claude_desktop_config.json`
**Windows**: `C:\Users\<username>\AppData\Roaming\Claude\claude_desktop_config.json`

```json
{
  "mcpServers": {
    "hasel": {
      "command": "node",
      "args": ["/absolute/path/to/os/mcp-server/dist/index.js"]
    }
  }
}
```

### Environment Variables (Optional)

Create a `.env` file in the `mcp-server` directory:

```env
# DuckDNS Token
DUCKDNS_TOKEN=your-token-here

# IPFS Configuration
IPFS_API_URL=http://127.0.0.1:5001

# Custom IPFS Gateways
IPFS_GATEWAYS=https://ipfs.io,https://gateway.pinata.cloud
```

## Available Tools

### 🌐 Web Tools

#### `web_fetch`

Fetch content from a URL with full HTTP control.

```json
{
  "url": "https://example.com",
  "method": "GET",
  "headers": {
    "Authorization": "Bearer token"
  },
  "timeout": 30000,
  "response_format": "markdown"
}
```

#### `web_scrape`

Scrape web content using Cheerio, Playwright, or Puppeteer.

```json
{
  "url": "https://example.com",
  "selector": ".content",
  "engine": "cheerio",
  "response_format": "json"
}
```

#### `web_crawl`

Crawl multiple pages starting from a URL.

```json
{
  "url": "https://example.com",
  "depth": 2,
  "maxPages": 10,
  "pattern": "https://example.com/.*",
  "response_format": "markdown"
}
```

### 🪐 IPFS Tools

#### `ipfs_add`

Add content to IPFS and get CID.

```json
{
  "content": "Hello, IPFS!",
  "filename": "hello.txt",
  "pin": true,
  "response_format": "json"
}
```

#### `ipfs_get`

Retrieve content from IPFS using CID.

```json
{
  "cid": "QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG",
  "timeout": 45000,
  "response_format": "markdown"
}
```

#### `ipfs_list_gateways`

List available IPFS gateways.

```json
{
  "response_format": "json"
}
```

### 🎭 Browser Automation

#### `browser_session`

Run a browser automation session.

```json
{
  "url": "https://example.com",
  "actions": [
    {
      "type": "click",
      "selector": "#login-button"
    },
    {
      "type": "type",
      "selector": "#username",
      "value": "user@example.com"
    },
    {
      "type": "screenshot"
    }
  ],
  "headless": true,
  "engine": "playwright",
  "response_format": "json"
}
```

### 🏠 DynDNS Tools

#### `dyndns_update`

Update dynamic DNS record.

```json
{
  "hostname": "squirrel.duckdns.org",
  "provider": "duckdns",
  "token": "your-duckdns-token",
  "response_format": "markdown"
}
```

#### `dyndns_resolve`

Resolve DNS records for a hostname.

```json
{
  "hostname": "squirrel.duckdns.org",
  "type": "A",
  "response_format": "json"
}
```

### 📋 Clipboard Tools

#### `clipboard_write`

Write content to clipboard buffer with history.

```json
{
  "content": "Hello, clipboard!",
  "format": "text",
  "label": "greeting"
}
```

#### `clipboard_read`

Read from clipboard history.

```json
{
  "last": 5,
  "format": "text"
}
```

#### `clipboard_search`

Search clipboard history.

```json
{
  "query": "password",
  "limit": 10
}
```

### 🔺 Pipeline Tool

#### `nusskette_execute`

Execute a NussKette pipeline (chain multiple operations).

```json
{
  "name": "fetch-and-ipfs",
  "description": "Fetch content from URL and add to IPFS",
  "steps": [
    {
      "monad": "url",
      "action": "web_fetch",
      "params": {
        "url": "https://example.com/data.json"
      }
    },
    {
      "monad": "ipfs",
      "action": "ipfs_add",
      "params": {
        "content": "{{previous.data.body}}"
      }
    }
  ],
  "response_format": "json"
}
```

### 🐿️ System Tool

#### `system_status`

Get HASEL MCP Server status and capabilities.

```json
{
  "response_format": "markdown"
}
```

## Development

### Watch Mode

```bash
npm run watch
```

### Run Without Building

```bash
npm run dev
```

## Architecture

```
mcp-server/
├── src/
│   ├── index.ts         # Main MCP server
│   ├── hasel.ts         # HASEL Monad core
│   ├── types.ts         # TypeScript type definitions
│   ├── constants.ts     # Configuration constants
│   ├── schemas.ts       # Zod validation schemas
│   └── shared.ts        # Shared utilities
├── dist/                # Compiled JavaScript (generated)
├── package.json
├── tsconfig.json
└── README.md
```

## HASEL Monad Examples

### Basic Monad Usage

```typescript
import { Monads } from './hasel';

// Create a text monad
const textMonad = Monads.text('Hello, World!');

// Transform the value
const upperMonad = textMonad.map(text => text.toUpperCase());

// Unwrap the value
const result = upperMonad.unwrap(); // "HELLO, WORLD!"
```

### Chaining Operations

```typescript
import { NussKette, Monads } from './hasel';

const pipeline = new NussKette(Monads.text('https://example.com'))
  .pipe(async (monad) => {
    const url = monad.unwrap();
    const data = await fetchData(url);
    return Monads.mixed(data);
  })
  .pipe(async (monad) => {
    const data = monad.unwrap();
    const processed = processData(data);
    return Monads.text(processed);
  });

const result = await pipeline.run();
```

## Example Use Cases

### 1. Monitor and Update DynDNS

```json
{
  "name": "update-squirrel-dns",
  "steps": [
    {
      "monad": "dns",
      "action": "dyndns_update",
      "params": {
        "hostname": "squirrel.duckdns.org",
        "provider": "duckdns",
        "token": "your-token"
      }
    },
    {
      "monad": "dns",
      "action": "dyndns_resolve",
      "params": {
        "hostname": "squirrel.duckdns.org",
        "type": "A"
      }
    }
  ]
}
```

### 2. Scrape and Archive to IPFS

```json
{
  "name": "archive-to-ipfs",
  "steps": [
    {
      "monad": "url",
      "action": "web_scrape",
      "params": {
        "url": "https://example.com/article",
        "selector": "article"
      }
    },
    {
      "monad": "ipfs",
      "action": "ipfs_add",
      "params": {
        "content": "{{previous.data.content}}",
        "filename": "article.txt"
      }
    }
  ]
}
```

### 3. Automated Browser Testing

```json
{
  "url": "https://example.com",
  "actions": [
    {
      "type": "navigate",
      "url": "https://example.com/login"
    },
    {
      "type": "type",
      "selector": "#email",
      "value": "test@example.com"
    },
    {
      "type": "type",
      "selector": "#password",
      "value": "password123"
    },
    {
      "type": "click",
      "selector": "button[type='submit']"
    },
    {
      "type": "waitFor",
      "selector": ".dashboard"
    },
    {
      "type": "screenshot"
    }
  ]
}
```

## Troubleshooting

### Server Not Starting

1. Check that Node.js >= 18 is installed: `node --version`
2. Ensure dependencies are installed: `npm install`
3. Build the project: `npm run build`
4. Check Claude Desktop logs for error messages

### IPFS Operations Failing

1. If using a local IPFS node, ensure it's running: `ipfs daemon`
2. Check that IPFS API is accessible: `curl http://127.0.0.1:5001/api/v0/version`
3. Verify gateway URLs are accessible

### DynDNS Updates Not Working

1. Verify your API token is correct
2. Check that the hostname format matches the provider's requirements
3. Ensure your IP is publicly accessible

## Contributing

This project is part of the Squirrel OS ecosystem. Contributions are welcome!

## License

MIT

## Related Projects

- [Squirrel OS](https://github.com/go-xoxo/os) - The main Haskell-based LLM integration project
- [Edh Programming Language](https://github.com/complyue/tour) - Reactive event integration with Haskell

---

Made with 🌰 by the Squirrel OS team

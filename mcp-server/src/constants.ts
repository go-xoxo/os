/**
 * 🐿️ Constants and Configuration for HASEL MCP Server
 */

// ═══════════════════════════════════════════════════
// 🏷️ Server Metadata
// ═══════════════════════════════════════════════════

export const SERVER_INFO = {
  name: 'hasel-mcp-server',
  version: '1.0.0',
  displayName: '🐿️ HASEL Master MCP Server',
  description: 'Master MCP Server with DynDNS, IPFS, Web, Playwright, Puppeteer, and HASEL Monads',
  author: 'xoxo <fritsch.david@gmail.com>',
} as const;

// ═══════════════════════════════════════════════════
// 🌐 Web Configuration
// ═══════════════════════════════════════════════════

export const WEB_CONFIG = {
  defaultTimeout: 30000,
  maxTimeout: 120000,
  defaultUserAgent:
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
  maxRedirects: 5,
} as const;

// ═══════════════════════════════════════════════════
// 🪐 IPFS Configuration
// ═══════════════════════════════════════════════════

export const IPFS_CONFIG = {
  defaultGateways: [
    'https://ipfs.io',
    'https://gateway.pinata.cloud',
    'https://cloudflare-ipfs.com',
    'https://dweb.link',
    'https://w3s.link',
  ],
  localAPI: 'http://127.0.0.1:5001',
  defaultTimeout: 45000,
  maxTimeout: 120000,
  pinByDefault: true,
} as const;

// ═══════════════════════════════════════════════════
// 🎭 Browser Automation Configuration
// ═══════════════════════════════════════════════════

export const BROWSER_CONFIG = {
  defaultViewport: {
    width: 1280,
    height: 720,
  },
  maxViewport: {
    width: 3840,
    height: 2160,
  },
  minViewport: {
    width: 320,
    height: 240,
  },
  defaultTimeout: 10000,
  maxTimeout: 60000,
  headless: true,
  defaultEngine: 'playwright' as const,
  maxActionsPerSession: 20,
} as const;

// ═══════════════════════════════════════════════════
// 🏠 DynDNS Configuration
// ═══════════════════════════════════════════════════

export const DYNDNS_CONFIG = {
  providers: {
    duckdns: {
      updateUrl: 'https://www.duckdns.org/update',
      requiresToken: true,
    },
    noip: {
      updateUrl: 'https://dynupdate.no-ip.com/nic/update',
      requiresToken: true,
    },
    dyndns: {
      updateUrl: 'https://members.dyndns.org/v3/update',
      requiresToken: true,
    },
    cloudflare: {
      updateUrl: 'https://api.cloudflare.com/client/v4/zones',
      requiresToken: true,
    },
  },
  ipCheckServices: [
    'https://api.ipify.org',
    'https://icanhazip.com',
    'https://ifconfig.me/ip',
  ],
  defaultDNSResolver: '1.1.1.1',
} as const;

// ═══════════════════════════════════════════════════
// 📋 Clipboard Configuration
// ═══════════════════════════════════════════════════

export const CLIPBOARD_CONFIG = {
  maxHistorySize: 100,
  defaultReadCount: 1,
  maxReadCount: 100,
  defaultSearchLimit: 10,
  maxSearchLimit: 50,
} as const;

// ═══════════════════════════════════════════════════
// 🔺 Pipeline Configuration
// ═══════════════════════════════════════════════════

export const PIPELINE_CONFIG = {
  maxSteps: 10,
  defaultTimeout: 60000,
  maxTimeout: 300000,
} as const;

// ═══════════════════════════════════════════════════
// 🎨 Format Templates
// ═══════════════════════════════════════════════════

export const MARKDOWN_TEMPLATES = {
  success: (title: string, content: string) => `✅ **${title}**\n\n${content}`,
  error: (title: string, error: string) => `❌ **${title}**\n\n\`\`\`\n${error}\n\`\`\``,
  info: (title: string, content: string) => `ℹ️ **${title}**\n\n${content}`,
  warning: (title: string, content: string) => `⚠️ **${title}**\n\n${content}`,
} as const;

// ═══════════════════════════════════════════════════
// 🐿️ Monad Type Mappings
// ═══════════════════════════════════════════════════

export const MONAD_TOOL_MAPPING = {
  text: ['web_fetch', 'ipfs_get', 'clipboard_read'],
  binary: ['web_scrape', 'browser_screenshot'],
  mixed: ['web_scrape', 'ipfs_add', 'nusskette_execute'],
  url: ['web_fetch', 'web_scrape', 'web_crawl'],
  ipfs: ['ipfs_add', 'ipfs_get', 'ipfs_list_gateways'],
  dom: ['web_scrape', 'browser_session'],
  dns: ['dyndns_update', 'dyndns_resolve'],
  clipboard: ['clipboard_write', 'clipboard_read', 'clipboard_search'],
  io: ['system_status'],
} as const;

// ═══════════════════════════════════════════════════
// 🎯 Error Messages
// ═══════════════════════════════════════════════════

export const ERROR_MESSAGES = {
  INVALID_URL: 'Invalid URL provided',
  NETWORK_ERROR: 'Network error occurred',
  TIMEOUT: 'Operation timed out',
  IPFS_ERROR: 'IPFS operation failed',
  BROWSER_ERROR: 'Browser automation failed',
  DNS_ERROR: 'DNS operation failed',
  CLIPBOARD_ERROR: 'Clipboard operation failed',
  INVALID_PARAMS: 'Invalid parameters provided',
  UNKNOWN_ERROR: 'An unknown error occurred',
} as const;

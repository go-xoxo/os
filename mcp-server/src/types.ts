/**
 * 🐿️ Type Definitions for HASEL MCP Server
 */

// ═══════════════════════════════════════════════════
// 🌐 Web Types
// ═══════════════════════════════════════════════════

export interface WebFetchParams {
  url: string;
  method?: 'GET' | 'POST' | 'PUT' | 'DELETE';
  headers?: Record<string, string>;
  body?: string;
  timeout?: number;
  response_format?: 'json' | 'markdown';
}

export interface WebScrapeParams {
  url: string;
  selector?: string;
  waitFor?: string;
  screenshot?: boolean;
  javascript?: boolean;
  engine?: 'playwright' | 'puppeteer' | 'cheerio';
  response_format?: 'json' | 'markdown';
}

export interface WebCrawlParams {
  url: string;
  depth?: number;
  maxPages?: number;
  pattern?: string;
  extractLinks?: boolean;
  response_format?: 'json' | 'markdown';
}

// ═══════════════════════════════════════════════════
// 🪐 IPFS Types
// ═══════════════════════════════════════════════════

export interface IPFSAddParams {
  content: string;
  filename?: string;
  pin?: boolean;
  response_format?: 'json' | 'markdown';
}

export interface IPFSGetParams {
  cid: string;
  timeout?: number;
  response_format?: 'json' | 'markdown';
}

export interface IPFSListGatewaysParams {
  response_format?: 'json' | 'markdown';
}

// ═══════════════════════════════════════════════════
// 🎭 Browser Automation Types
// ═══════════════════════════════════════════════════

export type BrowserActionType =
  | 'navigate'
  | 'click'
  | 'type'
  | 'screenshot'
  | 'evaluate'
  | 'waitFor'
  | 'select'
  | 'scroll';

export interface BrowserAction {
  type: BrowserActionType;
  selector?: string;
  value?: string;
  url?: string;
  script?: string;
  timeout?: number;
}

export interface BrowserSessionParams {
  url: string;
  actions?: BrowserAction[];
  headless?: boolean;
  engine?: 'playwright' | 'puppeteer';
  viewport?: {
    width: number;
    height: number;
  };
  response_format?: 'json' | 'markdown';
}

// ═══════════════════════════════════════════════════
// 🏠 DynDNS Types
// ═══════════════════════════════════════════════════

export type DNSProvider = 'duckdns' | 'noip' | 'dyndns' | 'cloudflare' | 'custom';

export interface DynDNSUpdateParams {
  hostname: string;
  ip?: string;
  provider?: DNSProvider;
  token?: string;
  response_format?: 'json' | 'markdown';
}

export interface DynDNSResolveParams {
  hostname: string;
  type?: 'A' | 'AAAA' | 'CNAME' | 'MX' | 'TXT' | 'NS';
  response_format?: 'json' | 'markdown';
}

// ═══════════════════════════════════════════════════
// 📋 Clipboard Types
// ═══════════════════════════════════════════════════

export interface ClipboardWriteParams {
  content: string;
  format?: 'text' | 'html' | 'json';
  label?: string;
}

export interface ClipboardReadParams {
  last?: number;
  format?: 'text' | 'html' | 'json';
}

export interface ClipboardSearchParams {
  query: string;
  limit?: number;
}

export interface ClipboardEntry {
  content: string;
  format: string;
  label?: string;
  timestamp: number;
}

// ═══════════════════════════════════════════════════
// 🔺 Pipeline Types
// ═══════════════════════════════════════════════════

export interface PipelineStep {
  monad: string;
  action: string;
  params: Record<string, unknown>;
}

export interface NussKetteParams {
  name: string;
  description?: string;
  steps: PipelineStep[];
  response_format?: 'json' | 'markdown';
}

// ═══════════════════════════════════════════════════
// 🐿️ System Types
// ═══════════════════════════════════════════════════

export interface SystemStatusParams {
  response_format?: 'json' | 'markdown';
}

export interface ServerInfo {
  name: string;
  version: string;
  monadTypes: string[];
  toolCount: number;
  uptime: number;
  capabilities: {
    web: boolean;
    ipfs: boolean;
    browser: boolean;
    dns: boolean;
    clipboard: boolean;
    pipeline: boolean;
  };
}

// ═══════════════════════════════════════════════════
// 📊 Result Types
// ═══════════════════════════════════════════════════

export interface ToolResult {
  success: boolean;
  data?: unknown;
  error?: string;
  metadata?: Record<string, unknown>;
}

export interface FormattedResponse {
  content: Array<{ type: 'text'; text: string }>;
  isError?: boolean;
}

// 🐿️ Type Definitions for Eichhörnchen Master MCP

export type MonadType =
  | 'text'      // Pure text transformations
  | 'binary'    // Binary data (images, PDFs, etc.)
  | 'mixed'     // Text + Binary
  | 'url'       // Web resources
  | 'ipfs'      // IPFS distributed content
  | 'dom'       // Browser DOM
  | 'dns'       // DNS operations
  | 'clipboard' // Clipboard operations
  | 'io';       // General I/O

export type ResponseFormat = 'json' | 'markdown';

export type HTTPMethod = 'GET' | 'POST' | 'PUT' | 'DELETE';

export type ScrapingEngine = 'cheerio' | 'playwright' | 'puppeteer';

export type BrowserEngine = 'playwright' | 'puppeteer';

export type DNSProvider = 'duckdns' | 'noip' | 'dyndns' | 'cloudflare' | 'custom';

export type DNSRecordType = 'A' | 'AAAA' | 'CNAME' | 'MX' | 'TXT' | 'NS';

export type ClipboardFormat = 'text' | 'html' | 'json';

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

export interface BrowserViewport {
  width: number;
  height: number;
}

export interface PipelineStep {
  monad: MonadType;
  action: string;
  params: Record<string, unknown>;
}

export interface ServerConfig {
  ipfsGateway: string;
  ipfsAPI?: string;
  dyndnsProvider: DNSProvider;
  dyndnsToken?: string;
  clipboardMaxHistory: number;
  browserDefaultTimeout: number;
}

export interface ToolResult {
  success: boolean;
  data?: unknown;
  error?: string;
  metadata?: Record<string, unknown>;
}

export interface IPFSAddResult {
  cid: string;
  size: number;
  pinned: boolean;
}

export interface IPFSGetResult {
  content: string;
  cid: string;
  size: number;
}

export interface DNSRecord {
  type: DNSRecordType;
  name: string;
  value: string;
  ttl?: number;
}

export interface ClipboardEntry {
  content: string;
  format: ClipboardFormat;
  timestamp: number;
  label?: string;
}

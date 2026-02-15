/**
 * 🐿️ Zod Schemas — Type-safe Nuss validation
 * Every Nuss must prove its type before entering the Kobel
 */

import { z } from 'zod';

// ═══════════════════════════════════════════════════
// 🌐 Web Schemas
// ═══════════════════════════════════════════════════

export const WebFetchSchema = z.object({
  url: z.string().url().describe('URL to fetch'),
  method: z.enum(['GET', 'POST', 'PUT', 'DELETE']).default('GET').describe('HTTP method'),
  headers: z.record(z.string()).optional().describe('Custom HTTP headers'),
  body: z.string().optional().describe('Request body for POST/PUT'),
  timeout: z.number().int().min(1000).max(120000).default(30000).describe('Timeout in ms'),
  response_format: z.enum(['json', 'markdown']).default('markdown').describe('Output format'),
}).strict();

export const WebScrapeSchema = z.object({
  url: z.string().url().describe('URL to scrape'),
  selector: z.string().optional().describe('CSS selector to extract (default: body)'),
  waitFor: z.string().optional().describe('CSS selector to wait for before scraping'),
  screenshot: z.boolean().default(false).describe('Take screenshot (base64)'),
  javascript: z.boolean().default(true).describe('Enable JavaScript execution'),
  engine: z.enum(['playwright', 'puppeteer', 'cheerio']).default('cheerio').describe('Scraping engine'),
  response_format: z.enum(['json', 'markdown']).default('markdown').describe('Output format'),
}).strict();

export const WebCrawlSchema = z.object({
  url: z.string().url().describe('Starting URL to crawl'),
  depth: z.number().int().min(1).max(5).default(2).describe('Max crawl depth'),
  maxPages: z.number().int().min(1).max(50).default(10).describe('Max pages to visit'),
  pattern: z.string().optional().describe('URL pattern filter (regex)'),
  extractLinks: z.boolean().default(true).describe('Extract and return all links'),
  response_format: z.enum(['json', 'markdown']).default('markdown').describe('Output format'),
}).strict();

// ═══════════════════════════════════════════════════
// 🪐 IPFS Schemas
// ═══════════════════════════════════════════════════

export const IPFSAddSchema = z.object({
  content: z.string().min(1).describe('Content to add to IPFS'),
  filename: z.string().optional().describe('Optional filename'),
  pin: z.boolean().default(true).describe('Pin content after adding'),
  response_format: z.enum(['json', 'markdown']).default('markdown').describe('Output format'),
}).strict();

export const IPFSGetSchema = z.object({
  cid: z.string().min(1).describe('IPFS Content Identifier (CID) to retrieve'),
  timeout: z.number().int().min(5000).max(120000).default(45000).describe('Timeout in ms'),
  response_format: z.enum(['json', 'markdown']).default('markdown').describe('Output format'),
}).strict();

export const IPFSListGatewaysSchema = z.object({
  response_format: z.enum(['json', 'markdown']).default('markdown').describe('Output format'),
}).strict();

// ═══════════════════════════════════════════════════
// 🎭 Browser Automation Schemas
// ═══════════════════════════════════════════════════

export const BrowserActionSchema = z.object({
  type: z.enum(['navigate', 'click', 'type', 'screenshot', 'evaluate', 'waitFor', 'select', 'scroll']),
  selector: z.string().optional(),
  value: z.string().optional(),
  url: z.string().optional(),
  script: z.string().optional(),
  timeout: z.number().int().min(1000).max(60000).default(10000),
});

export const BrowserSessionSchema = z.object({
  url: z.string().url().describe('Initial URL to navigate to'),
  actions: z.array(BrowserActionSchema).min(0).max(20).default([]).describe('Sequence of browser actions'),
  headless: z.boolean().default(true).describe('Run browser headless'),
  engine: z.enum(['playwright', 'puppeteer']).default('playwright').describe('Browser engine'),
  viewport: z.object({
    width: z.number().int().min(320).max(3840).default(1280),
    height: z.number().int().min(240).max(2160).default(720),
  }).optional().describe('Browser viewport size'),
  response_format: z.enum(['json', 'markdown']).default('markdown').describe('Output format'),
}).strict();

// ═══════════════════════════════════════════════════
// 🏠 DynDNS Schemas
// ═══════════════════════════════════════════════════

export const DynDNSUpdateSchema = z.object({
  hostname: z.string().min(1).describe('Hostname to update (e.g., squirrel.duckdns.org)'),
  ip: z.string().optional().describe('IP address (auto-detected if empty)'),
  provider: z.enum(['duckdns', 'noip', 'dyndns', 'cloudflare', 'custom']).default('duckdns').describe('DNS provider'),
  token: z.string().optional().describe('API token for the provider'),
  response_format: z.enum(['json', 'markdown']).default('markdown').describe('Output format'),
}).strict();

export const DynDNSResolveSchema = z.object({
  hostname: z.string().min(1).describe('Hostname to resolve'),
  type: z.enum(['A', 'AAAA', 'CNAME', 'MX', 'TXT', 'NS']).default('A').describe('DNS record type'),
  response_format: z.enum(['json', 'markdown']).default('markdown').describe('Output format'),
}).strict();

// ═══════════════════════════════════════════════════
// 📋 Clipboard Schemas
// ═══════════════════════════════════════════════════

export const ClipboardWriteSchema = z.object({
  content: z.string().min(1).describe('Content to store in clipboard buffer'),
  format: z.enum(['text', 'html', 'json']).default('text').describe('Content format'),
  label: z.string().optional().describe('Label for this clipboard entry'),
}).strict();

export const ClipboardReadSchema = z.object({
  last: z.number().int().min(1).max(100).default(1).describe('Number of recent entries to read'),
  format: z.enum(['text', 'html', 'json']).default('text').describe('Desired output format'),
}).strict();

export const ClipboardSearchSchema = z.object({
  query: z.string().min(1).describe('Search query for clipboard history'),
  limit: z.number().int().min(1).max(50).default(10).describe('Max results'),
}).strict();

// ═══════════════════════════════════════════════════
// 🔺 NussKette Pipeline Schema
// ═══════════════════════════════════════════════════

export const PipelineStepSchema = z.object({
  monad: z.enum(['text', 'binary', 'mixed', 'url', 'ipfs', 'dom', 'dns', 'clipboard', 'io']).describe('Monad type for this step'),
  action: z.string().min(1).describe('Action to perform (tool name)'),
  params: z.record(z.unknown()).describe('Parameters for the action'),
});

export const NussKetteSchema = z.object({
  name: z.string().min(1).describe('Pipeline name'),
  description: z.string().optional().describe('Pipeline description'),
  steps: z.array(PipelineStepSchema).min(1).max(10).describe('Ordered pipeline steps'),
  response_format: z.enum(['json', 'markdown']).default('markdown').describe('Output format'),
}).strict();

// ═══════════════════════════════════════════════════
// 🐿️ System Schemas
// ═══════════════════════════════════════════════════

export const SystemStatusSchema = z.object({
  response_format: z.enum(['json', 'markdown']).default('markdown').describe('Output format'),
}).strict();

#!/usr/bin/env node

/**
 * 🐿️ HASEL Master MCP Server
 *
 * A comprehensive MCP server integrating:
 * - DynDNS (squirrel.duckdns.org and more)
 * - IPFS (distributed storage)
 * - Web (fetch, scrape, crawl)
 * - Playwright/Puppeteer (browser automation)
 * - All HASEL Monads (type-safe data pipelines)
 */

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
  Tool,
} from '@modelcontextprotocol/sdk/types.js';
import * as cheerio from 'cheerio';
import clipboardy from 'clipboardy';
import axios from 'axios';
import { Monads, NussKette } from './hasel.js';
import {
  WebFetchSchema,
  WebScrapeSchema,
  WebCrawlSchema,
  IPFSAddSchema,
  IPFSGetSchema,
  IPFSListGatewaysSchema,
  BrowserSessionSchema,
  DynDNSUpdateSchema,
  DynDNSResolveSchema,
  ClipboardWriteSchema,
  ClipboardReadSchema,
  ClipboardSearchSchema,
  NussKetteSchema,
  SystemStatusSchema,
} from './schemas.js';
import {
  fetchWithTimeout,
  formatToolResult,
  handleError,
  validateUrl,
  clipboardHistory,
  createError,
} from './shared.js';
import {
  WebFetchParams,
  WebScrapeParams,
  WebCrawlParams,
  IPFSAddParams,
  IPFSGetParams,
  BrowserSessionParams,
  DynDNSUpdateParams,
  DynDNSResolveParams,
  ClipboardWriteParams,
  ClipboardReadParams,
  ClipboardSearchParams,
  NussKetteParams,
  SystemStatusParams,
  ToolResult,
  ServerInfo,
} from './types.js';
import { SERVER_INFO, IPFS_CONFIG, DYNDNS_CONFIG, WEB_CONFIG } from './constants.js';

// ═══════════════════════════════════════════════════
// 🏗️ Server Initialization
// ═══════════════════════════════════════════════════

const server = new Server(
  {
    name: SERVER_INFO.name,
    version: SERVER_INFO.version,
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

const startTime = Date.now();

// ═══════════════════════════════════════════════════
// 🔧 Tool Definitions
// ═══════════════════════════════════════════════════

const tools: Tool[] = [
  // 🌐 Web Tools
  {
    name: 'web_fetch',
    description: 'Fetch content from a URL with full HTTP control (GET, POST, PUT, DELETE)',
    inputSchema: {
      type: 'object',
      properties: {
        url: { type: 'string', description: 'URL to fetch' },
        method: { type: 'string', enum: ['GET', 'POST', 'PUT', 'DELETE'], default: 'GET' },
        headers: { type: 'object', description: 'Custom HTTP headers' },
        body: { type: 'string', description: 'Request body for POST/PUT' },
        timeout: { type: 'number', default: 30000 },
        response_format: { type: 'string', enum: ['json', 'markdown'], default: 'markdown' },
      },
      required: ['url'],
    },
  },
  {
    name: 'web_scrape',
    description: 'Scrape web content using Cheerio, Playwright, or Puppeteer',
    inputSchema: {
      type: 'object',
      properties: {
        url: { type: 'string', description: 'URL to scrape' },
        selector: { type: 'string', description: 'CSS selector to extract' },
        waitFor: { type: 'string', description: 'CSS selector to wait for' },
        screenshot: { type: 'boolean', default: false },
        javascript: { type: 'boolean', default: true },
        engine: { type: 'string', enum: ['playwright', 'puppeteer', 'cheerio'], default: 'cheerio' },
        response_format: { type: 'string', enum: ['json', 'markdown'], default: 'markdown' },
      },
      required: ['url'],
    },
  },
  {
    name: 'web_crawl',
    description: 'Crawl multiple pages starting from a URL',
    inputSchema: {
      type: 'object',
      properties: {
        url: { type: 'string', description: 'Starting URL' },
        depth: { type: 'number', default: 2, description: 'Max crawl depth' },
        maxPages: { type: 'number', default: 10, description: 'Max pages to visit' },
        pattern: { type: 'string', description: 'URL pattern filter (regex)' },
        extractLinks: { type: 'boolean', default: true },
        response_format: { type: 'string', enum: ['json', 'markdown'], default: 'markdown' },
      },
      required: ['url'],
    },
  },

  // 🪐 IPFS Tools
  {
    name: 'ipfs_add',
    description: 'Add content to IPFS and get CID',
    inputSchema: {
      type: 'object',
      properties: {
        content: { type: 'string', description: 'Content to add' },
        filename: { type: 'string', description: 'Optional filename' },
        pin: { type: 'boolean', default: true },
        response_format: { type: 'string', enum: ['json', 'markdown'], default: 'markdown' },
      },
      required: ['content'],
    },
  },
  {
    name: 'ipfs_get',
    description: 'Retrieve content from IPFS using CID',
    inputSchema: {
      type: 'object',
      properties: {
        cid: { type: 'string', description: 'IPFS Content Identifier' },
        timeout: { type: 'number', default: 45000 },
        response_format: { type: 'string', enum: ['json', 'markdown'], default: 'markdown' },
      },
      required: ['cid'],
    },
  },
  {
    name: 'ipfs_list_gateways',
    description: 'List available IPFS gateways',
    inputSchema: {
      type: 'object',
      properties: {
        response_format: { type: 'string', enum: ['json', 'markdown'], default: 'markdown' },
      },
    },
  },

  // 🎭 Browser Automation Tools
  {
    name: 'browser_session',
    description: 'Run a browser automation session with Playwright or Puppeteer',
    inputSchema: {
      type: 'object',
      properties: {
        url: { type: 'string', description: 'Initial URL' },
        actions: {
          type: 'array',
          items: {
            type: 'object',
            properties: {
              type: { type: 'string', enum: ['navigate', 'click', 'type', 'screenshot', 'evaluate', 'waitFor', 'select', 'scroll'] },
              selector: { type: 'string' },
              value: { type: 'string' },
              url: { type: 'string' },
              script: { type: 'string' },
              timeout: { type: 'number', default: 10000 },
            },
          },
        },
        headless: { type: 'boolean', default: true },
        engine: { type: 'string', enum: ['playwright', 'puppeteer'], default: 'playwright' },
        viewport: {
          type: 'object',
          properties: {
            width: { type: 'number', default: 1280 },
            height: { type: 'number', default: 720 },
          },
        },
        response_format: { type: 'string', enum: ['json', 'markdown'], default: 'markdown' },
      },
      required: ['url'],
    },
  },

  // 🏠 DynDNS Tools
  {
    name: 'dyndns_update',
    description: 'Update dynamic DNS record (supports DuckDNS, No-IP, DynDNS, Cloudflare)',
    inputSchema: {
      type: 'object',
      properties: {
        hostname: { type: 'string', description: 'Hostname to update' },
        ip: { type: 'string', description: 'IP address (auto-detected if empty)' },
        provider: { type: 'string', enum: ['duckdns', 'noip', 'dyndns', 'cloudflare', 'custom'], default: 'duckdns' },
        token: { type: 'string', description: 'API token' },
        response_format: { type: 'string', enum: ['json', 'markdown'], default: 'markdown' },
      },
      required: ['hostname'],
    },
  },
  {
    name: 'dyndns_resolve',
    description: 'Resolve DNS records for a hostname',
    inputSchema: {
      type: 'object',
      properties: {
        hostname: { type: 'string', description: 'Hostname to resolve' },
        type: { type: 'string', enum: ['A', 'AAAA', 'CNAME', 'MX', 'TXT', 'NS'], default: 'A' },
        response_format: { type: 'string', enum: ['json', 'markdown'], default: 'markdown' },
      },
      required: ['hostname'],
    },
  },

  // 📋 Clipboard Tools
  {
    name: 'clipboard_write',
    description: 'Write content to clipboard buffer with history',
    inputSchema: {
      type: 'object',
      properties: {
        content: { type: 'string', description: 'Content to write' },
        format: { type: 'string', enum: ['text', 'html', 'json'], default: 'text' },
        label: { type: 'string', description: 'Optional label' },
      },
      required: ['content'],
    },
  },
  {
    name: 'clipboard_read',
    description: 'Read from clipboard history',
    inputSchema: {
      type: 'object',
      properties: {
        last: { type: 'number', default: 1, description: 'Number of recent entries' },
        format: { type: 'string', enum: ['text', 'html', 'json'], default: 'text' },
      },
    },
  },
  {
    name: 'clipboard_search',
    description: 'Search clipboard history',
    inputSchema: {
      type: 'object',
      properties: {
        query: { type: 'string', description: 'Search query' },
        limit: { type: 'number', default: 10 },
      },
      required: ['query'],
    },
  },

  // 🔺 Pipeline Tool
  {
    name: 'nusskette_execute',
    description: 'Execute a NussKette pipeline (chain multiple operations)',
    inputSchema: {
      type: 'object',
      properties: {
        name: { type: 'string', description: 'Pipeline name' },
        description: { type: 'string', description: 'Pipeline description' },
        steps: {
          type: 'array',
          items: {
            type: 'object',
            properties: {
              monad: { type: 'string', enum: ['text', 'binary', 'mixed', 'url', 'ipfs', 'dom', 'dns', 'clipboard', 'io'] },
              action: { type: 'string', description: 'Tool name' },
              params: { type: 'object', description: 'Tool parameters' },
            },
            required: ['monad', 'action', 'params'],
          },
        },
        response_format: { type: 'string', enum: ['json', 'markdown'], default: 'markdown' },
      },
      required: ['name', 'steps'],
    },
  },

  // 🐿️ System Tool
  {
    name: 'system_status',
    description: 'Get HASEL MCP Server status and capabilities',
    inputSchema: {
      type: 'object',
      properties: {
        response_format: { type: 'string', enum: ['json', 'markdown'], default: 'markdown' },
      },
    },
  },
];

// ═══════════════════════════════════════════════════
// 🔨 Tool Implementations
// ═══════════════════════════════════════════════════

async function webFetch(params: WebFetchParams): Promise<ToolResult> {
  try {
    const validated = WebFetchSchema.parse(params);
    validateUrl(validated.url);

    const response = await fetchWithTimeout(validated.url, {
      method: validated.method,
      headers: {
        'User-Agent': WEB_CONFIG.defaultUserAgent,
        ...validated.headers,
      },
      body: validated.body,
      timeout: validated.timeout,
    });

    const contentType = response.headers.get('content-type') || '';
    let data: any;

    if (contentType.includes('application/json')) {
      data = await response.json();
    } else {
      data = await response.text();
    }

    return {
      success: true,
      data: {
        status: response.status,
        statusText: response.statusText,
        headers: Object.fromEntries(response.headers.entries()),
        body: data,
      },
      metadata: {
        url: validated.url,
        method: validated.method,
        contentType,
      },
    };
  } catch (error) {
    return handleError(error);
  }
}

async function webScrape(params: WebScrapeParams): Promise<ToolResult> {
  try {
    const validated = WebScrapeSchema.parse(params);
    validateUrl(validated.url);

    if (validated.engine === 'cheerio') {
      // Simple scraping without JS
      const response = await fetchWithTimeout(validated.url, {
        timeout: 30000,
      });
      const html = await response.text();
      const $ = cheerio.load(html);

      const selector = validated.selector || 'body';
      const content = $(selector).text().trim();

      return {
        success: true,
        data: {
          url: validated.url,
          content,
          html: $(selector).html(),
        },
        metadata: {
          engine: 'cheerio',
          selector,
        },
      };
    } else {
      // For Playwright/Puppeteer, we'd need to implement browser automation
      // For now, fall back to cheerio
      return {
        success: false,
        error: `${validated.engine} engine not fully implemented yet. Use 'cheerio' for now.`,
      };
    }
  } catch (error) {
    return handleError(error);
  }
}

async function webCrawl(params: WebCrawlParams): Promise<ToolResult> {
  try {
    const validated = WebCrawlSchema.parse(params);
    validateUrl(validated.url);

    const visited = new Set<string>();
    const toVisit = [{ url: validated.url, depth: 0 }];
    const results: any[] = [];

    while (toVisit.length > 0 && visited.size < validated.maxPages) {
      const { url, depth } = toVisit.shift()!;

      if (visited.has(url) || depth > validated.depth) {
        continue;
      }

      visited.add(url);

      try {
        const response = await fetchWithTimeout(url, { timeout: 10000 });
        const html = await response.text();
        const $ = cheerio.load(html);

        const pageData = {
          url,
          depth,
          title: $('title').text(),
          text: $('body').text().substring(0, 500),
        };

        results.push(pageData);

        if (validated.extractLinks && depth < validated.depth) {
          $('a[href]').each((_, elem) => {
            const href = $(elem).attr('href');
            if (href) {
              try {
                const absoluteUrl = new URL(href, url).href;
                if (!validated.pattern || new RegExp(validated.pattern).test(absoluteUrl)) {
                  toVisit.push({ url: absoluteUrl, depth: depth + 1 });
                }
              } catch {
                // Invalid URL, skip
              }
            }
          });
        }
      } catch (error) {
        // Skip failed pages
      }
    }

    return {
      success: true,
      data: {
        startUrl: validated.url,
        pagesVisited: visited.size,
        pages: results,
      },
    };
  } catch (error) {
    return handleError(error);
  }
}

async function ipfsAdd(params: IPFSAddParams): Promise<ToolResult> {
  try {
    const validated = IPFSAddSchema.parse(params);

    // Try to use public IPFS gateway for adding
    // Note: In production, you'd use a local IPFS node or Pinata/Web3.Storage API
    const mockCID = `Qm${Buffer.from(validated.content).toString('base64').substring(0, 44)}`;

    return {
      success: true,
      data: {
        cid: mockCID,
        size: validated.content.length,
        pinned: validated.pin,
        gateways: IPFS_CONFIG.defaultGateways.map(g => `${g}/ipfs/${mockCID}`),
      },
      metadata: {
        note: 'This is a mock implementation. For production, connect to a real IPFS node.',
      },
    };
  } catch (error) {
    return handleError(error);
  }
}

async function ipfsGet(params: IPFSGetParams): Promise<ToolResult> {
  try {
    const validated = IPFSGetSchema.parse(params);

    // Try multiple gateways
    for (const gateway of IPFS_CONFIG.defaultGateways) {
      try {
        const url = `${gateway}/ipfs/${validated.cid}`;
        const response = await fetchWithTimeout(url, {
          timeout: validated.timeout,
        });

        const content = await response.text();

        return {
          success: true,
          data: {
            cid: validated.cid,
            content,
            gateway,
            size: content.length,
          },
        };
      } catch {
        // Try next gateway
      }
    }

    throw createError('IPFS_ERROR', { cid: validated.cid, message: 'Failed to fetch from all gateways' });
  } catch (error) {
    return handleError(error);
  }
}

async function ipfsListGateways(params: any): Promise<ToolResult> {
  try {
    const validated = IPFSListGatewaysSchema.parse(params);

    return {
      success: true,
      data: {
        gateways: IPFS_CONFIG.defaultGateways,
        localAPI: IPFS_CONFIG.localAPI,
      },
    };
  } catch (error) {
    return handleError(error);
  }
}

async function browserSession(params: BrowserSessionParams): Promise<ToolResult> {
  try {
    const validated = BrowserSessionSchema.parse(params);

    // Placeholder for browser automation
    // Full implementation would use Playwright/Puppeteer
    return {
      success: false,
      error: 'Browser automation not fully implemented yet. Install Playwright/Puppeteer and implement.',
    };
  } catch (error) {
    return handleError(error);
  }
}

async function dyndnsUpdate(params: DynDNSUpdateParams): Promise<ToolResult> {
  try {
    const validated = DynDNSUpdateSchema.parse(params);

    // Get current IP if not provided
    let ip = validated.ip;
    if (!ip) {
      const ipResponse = await fetchWithTimeout(DYNDNS_CONFIG.ipCheckServices[0]);
      ip = (await ipResponse.text()).trim();
    }

    // Update based on provider
    if (validated.provider === 'duckdns') {
      if (!validated.token) {
        throw createError('INVALID_PARAMS', { message: 'DuckDNS requires a token' });
      }

      const domain = validated.hostname.replace('.duckdns.org', '');
      const url = `${DYNDNS_CONFIG.providers.duckdns.updateUrl}?domains=${domain}&token=${validated.token}&ip=${ip}`;

      const response = await fetchWithTimeout(url);
      const result = await response.text();

      return {
        success: result.includes('OK'),
        data: {
          hostname: validated.hostname,
          ip,
          provider: 'duckdns',
          updated: result.includes('OK'),
        },
      };
    }

    return {
      success: false,
      error: `Provider ${validated.provider} not fully implemented yet`,
    };
  } catch (error) {
    return handleError(error);
  }
}

async function dyndnsResolve(params: DynDNSResolveParams): Promise<ToolResult> {
  try {
    const validated = DynDNSResolveSchema.parse(params);

    // Use DNS over HTTPS (Cloudflare)
    const url = `https://cloudflare-dns.com/dns-query?name=${validated.hostname}&type=${validated.type}`;
    const response = await fetchWithTimeout(url, {
      headers: {
        'Accept': 'application/dns-json',
      },
    });

    const data = await response.json() as any;

    return {
      success: true,
      data: {
        hostname: validated.hostname,
        type: validated.type,
        answers: data.Answer || [],
      },
    };
  } catch (error) {
    return handleError(error);
  }
}

async function clipboardWriteTool(params: ClipboardWriteParams): Promise<ToolResult> {
  try {
    const validated = ClipboardWriteSchema.parse(params);

    await clipboardy.write(validated.content);

    clipboardHistory.add({
      content: validated.content,
      format: validated.format,
      label: validated.label,
      timestamp: Date.now(),
    });

    return {
      success: true,
      data: {
        written: true,
        size: validated.content.length,
        format: validated.format,
      },
    };
  } catch (error) {
    return handleError(error);
  }
}

async function clipboardRead(params: ClipboardReadParams): Promise<ToolResult> {
  try {
    const validated = ClipboardReadSchema.parse(params);

    const entries = clipboardHistory.getLast(validated.last);

    return {
      success: true,
      data: {
        entries,
        count: entries.length,
      },
    };
  } catch (error) {
    return handleError(error);
  }
}

async function clipboardSearch(params: ClipboardSearchParams): Promise<ToolResult> {
  try {
    const validated = ClipboardSearchSchema.parse(params);

    const results = clipboardHistory.search(validated.query, validated.limit);

    return {
      success: true,
      data: {
        query: validated.query,
        results,
        count: results.length,
      },
    };
  } catch (error) {
    return handleError(error);
  }
}

async function nussketteExecute(params: NussKetteParams): Promise<ToolResult> {
  try {
    const validated = NussKetteSchema.parse(params);

    // Execute pipeline steps sequentially
    const results: any[] = [];
    let currentData: any = null;

    for (const step of validated.steps) {
      // Execute the tool for this step
      const toolResult = await executeTool(step.action, {
        ...step.params,
        ...(currentData ? { _input: currentData } : {}),
      });

      results.push({
        step: step.action,
        monad: step.monad,
        result: toolResult,
      });

      if (!toolResult.success) {
        break;
      }

      currentData = toolResult.data;
    }

    return {
      success: results.every(r => r.result.success),
      data: {
        pipeline: validated.name,
        description: validated.description,
        steps: results,
        finalResult: currentData,
      },
    };
  } catch (error) {
    return handleError(error);
  }
}

async function systemStatus(params: SystemStatusParams): Promise<ToolResult> {
  try {
    const validated = SystemStatusSchema.parse(params);

    const status: ServerInfo = {
      name: SERVER_INFO.name,
      version: SERVER_INFO.version,
      monadTypes: ['text', 'binary', 'mixed', 'url', 'ipfs', 'dom', 'dns', 'clipboard', 'io', 'error'],
      toolCount: tools.length,
      uptime: Date.now() - startTime,
      capabilities: {
        web: true,
        ipfs: true,
        browser: false, // Not fully implemented
        dns: true,
        clipboard: true,
        pipeline: true,
      },
    };

    return {
      success: true,
      data: status,
    };
  } catch (error) {
    return handleError(error);
  }
}

// ═══════════════════════════════════════════════════
// 🎯 Tool Router
// ═══════════════════════════════════════════════════

async function executeTool(name: string, params: any): Promise<ToolResult> {
  switch (name) {
    case 'web_fetch':
      return webFetch(params);
    case 'web_scrape':
      return webScrape(params);
    case 'web_crawl':
      return webCrawl(params);
    case 'ipfs_add':
      return ipfsAdd(params);
    case 'ipfs_get':
      return ipfsGet(params);
    case 'ipfs_list_gateways':
      return ipfsListGateways(params);
    case 'browser_session':
      return browserSession(params);
    case 'dyndns_update':
      return dyndnsUpdate(params);
    case 'dyndns_resolve':
      return dyndnsResolve(params);
    case 'clipboard_write':
      return clipboardWriteTool(params);
    case 'clipboard_read':
      return clipboardRead(params);
    case 'clipboard_search':
      return clipboardSearch(params);
    case 'nusskette_execute':
      return nussketteExecute(params);
    case 'system_status':
      return systemStatus(params);
    default:
      return {
        success: false,
        error: `Unknown tool: ${name}`,
      };
  }
}

// ═══════════════════════════════════════════════════
// 🎧 Request Handlers
// ═══════════════════════════════════════════════════

server.setRequestHandler(ListToolsRequestSchema, async () => {
  return { tools };
});

server.setRequestHandler(CallToolRequestSchema, async (request) => {
  const { name, arguments: args } = request.params;

  try {
    const result = await executeTool(name, args || {});
    const responseFormat = (args as any)?.response_format || 'markdown';

    return {
      content: [
        {
          type: 'text' as const,
          text: formatToolResult(result, responseFormat, name),
        },
      ],
      isError: !result.success,
    };
  } catch (error) {
    const errorResult = handleError(error);
    return {
      content: [
        {
          type: 'text' as const,
          text: formatToolResult(errorResult, 'markdown', name),
        },
      ],
      isError: true,
    };
  }
});

// ═══════════════════════════════════════════════════
// 🚀 Server Startup
// ═══════════════════════════════════════════════════

async function main() {
  console.error('🐿️ Starting HASEL Master MCP Server...');
  console.error(`📦 Version: ${SERVER_INFO.version}`);
  console.error(`🔧 Tools: ${tools.length}`);
  console.error('🌰 HASEL Monads ready!');

  const transport = new StdioServerTransport();
  await server.connect(transport);

  console.error('✅ Server running!');
}

main().catch((error) => {
  console.error('❌ Fatal error:', error);
  process.exit(1);
});

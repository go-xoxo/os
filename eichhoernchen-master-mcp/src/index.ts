#!/usr/bin/env node

// 🐿️ Eichhörnchen Master MCP Server
// The ultimate MCP server combining HASEL monads, Web, IPFS, Browser, DNS, and more!

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
  Tool,
} from '@modelcontextprotocol/sdk/types.js';

import { SERVER_NAME, SERVER_VERSION, SERVER_DESCRIPTION } from './constants.js';
import type { ToolResult } from './types.js';

// Import all tool implementations
import { webFetch, webScrape, webCrawl } from './tools/web.js';
import { ipfsAdd, ipfsGet, ipfsListGateways } from './tools/ipfs.js';
import { browserSession } from './tools/browser.js';
import { dyndnsUpdate, dyndnsResolve } from './tools/dns.js';
import { clipboardWrite, clipboardRead, clipboardSearch } from './tools/clipboard.js';
import { nusskette, type ToolExecutor } from './tools/pipeline.js';
import { systemStatus } from './tools/system.js';

// Import schemas
import * as schemas from './schemas.js';

/**
 * Main MCP Server
 */
class EichhoernchenMCPServer implements ToolExecutor {
  private server: Server;

  constructor() {
    this.server = new Server(
      {
        name: SERVER_NAME,
        version: SERVER_VERSION,
      },
      {
        capabilities: {
          tools: {},
        },
      }
    );

    this.setupHandlers();
  }

  private setupHandlers() {
    // List available tools
    this.server.setRequestHandler(ListToolsRequestSchema, async () => {
      return {
        tools: this.getTools(),
      };
    });

    // Execute tools
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      try {
        const result = await this.executeTool(request.params.name, request.params.arguments);

        if (!result.success) {
          return {
            content: [
              {
                type: 'text',
                text: `Error: ${result.error}`,
              },
            ],
            isError: true,
          };
        }

        return {
          content: [
            {
              type: 'text',
              text: typeof result.data === 'string' ? result.data : JSON.stringify(result.data, null, 2),
            },
          ],
        };
      } catch (error) {
        return {
          content: [
            {
              type: 'text',
              text: `Error executing tool: ${error instanceof Error ? error.message : String(error)}`,
            },
          ],
          isError: true,
        };
      }
    });
  }

  /**
   * Execute a tool by name
   */
  async executeTool(name: string, params: unknown): Promise<ToolResult> {
    try {
      switch (name) {
        // Web tools
        case 'web_fetch':
          return await webFetch(schemas.WebFetchSchema.parse(params));
        case 'web_scrape':
          return await webScrape(schemas.WebScrapeSchema.parse(params));
        case 'web_crawl':
          return await webCrawl(schemas.WebCrawlSchema.parse(params));

        // IPFS tools
        case 'ipfs_add':
          return await ipfsAdd(schemas.IPFSAddSchema.parse(params));
        case 'ipfs_get':
          return await ipfsGet(schemas.IPFSGetSchema.parse(params));
        case 'ipfs_list_gateways':
          return await ipfsListGateways(schemas.IPFSListGatewaysSchema.parse(params));

        // Browser tools
        case 'browser_session':
          return await browserSession(schemas.BrowserSessionSchema.parse(params));

        // DNS tools
        case 'dyndns_update':
          return await dyndnsUpdate(schemas.DynDNSUpdateSchema.parse(params));
        case 'dyndns_resolve':
          return await dyndnsResolve(schemas.DynDNSResolveSchema.parse(params));

        // Clipboard tools
        case 'clipboard_write':
          return await clipboardWrite(schemas.ClipboardWriteSchema.parse(params));
        case 'clipboard_read':
          return await clipboardRead(schemas.ClipboardReadSchema.parse(params));
        case 'clipboard_search':
          return await clipboardSearch(schemas.ClipboardSearchSchema.parse(params));

        // Pipeline tools
        case 'nusskette_run':
          return await nusskette(schemas.NussKetteSchema.parse(params), this);

        // System tools
        case 'system_status':
          return await systemStatus(schemas.SystemStatusSchema.parse(params));

        default:
          return {
            success: false,
            error: `Unknown tool: ${name}`,
          };
      }
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  /**
   * Get all available tools
   */
  private getTools(): Tool[] {
    return [
      // Web Tools
      {
        name: 'web_fetch',
        description: 'Fetch content from a URL using HTTP methods (GET, POST, PUT, DELETE)',
        inputSchema: {
          type: 'object',
          properties: {
            url: { type: 'string', description: 'URL to fetch' },
            method: {
              type: 'string',
              enum: ['GET', 'POST', 'PUT', 'DELETE'],
              default: 'GET',
              description: 'HTTP method',
            },
            headers: {
              type: 'object',
              additionalProperties: { type: 'string' },
              description: 'Custom HTTP headers',
            },
            body: { type: 'string', description: 'Request body for POST/PUT' },
            timeout: {
              type: 'number',
              default: 30000,
              description: 'Timeout in milliseconds',
            },
            response_format: {
              type: 'string',
              enum: ['json', 'markdown'],
              default: 'markdown',
              description: 'Output format',
            },
          },
          required: ['url'],
        },
      },
      {
        name: 'web_scrape',
        description: 'Scrape content from a web page using CSS selectors (Cheerio, Playwright, or Puppeteer)',
        inputSchema: {
          type: 'object',
          properties: {
            url: { type: 'string', description: 'URL to scrape' },
            selector: {
              type: 'string',
              description: 'CSS selector to extract (default: body)',
            },
            waitFor: {
              type: 'string',
              description: 'CSS selector to wait for before scraping',
            },
            screenshot: {
              type: 'boolean',
              default: false,
              description: 'Take screenshot',
            },
            javascript: {
              type: 'boolean',
              default: true,
              description: 'Enable JavaScript',
            },
            engine: {
              type: 'string',
              enum: ['cheerio', 'playwright', 'puppeteer'],
              default: 'cheerio',
              description: 'Scraping engine',
            },
            response_format: {
              type: 'string',
              enum: ['json', 'markdown'],
              default: 'markdown',
            },
          },
          required: ['url'],
        },
      },
      {
        name: 'web_crawl',
        description: 'Recursively crawl a website, following links up to a specified depth',
        inputSchema: {
          type: 'object',
          properties: {
            url: { type: 'string', description: 'Starting URL to crawl' },
            depth: {
              type: 'number',
              default: 2,
              description: 'Maximum crawl depth',
            },
            maxPages: {
              type: 'number',
              default: 10,
              description: 'Maximum pages to visit',
            },
            pattern: {
              type: 'string',
              description: 'URL pattern filter (regex)',
            },
            extractLinks: {
              type: 'boolean',
              default: true,
              description: 'Extract and return all links',
            },
            response_format: {
              type: 'string',
              enum: ['json', 'markdown'],
              default: 'markdown',
            },
          },
          required: ['url'],
        },
      },

      // IPFS Tools
      {
        name: 'ipfs_add',
        description: 'Add content to IPFS and get the CID',
        inputSchema: {
          type: 'object',
          properties: {
            content: { type: 'string', description: 'Content to add to IPFS' },
            filename: { type: 'string', description: 'Optional filename' },
            pin: { type: 'boolean', default: true, description: 'Pin content' },
            response_format: {
              type: 'string',
              enum: ['json', 'markdown'],
              default: 'markdown',
            },
          },
          required: ['content'],
        },
      },
      {
        name: 'ipfs_get',
        description: 'Retrieve content from IPFS by CID',
        inputSchema: {
          type: 'object',
          properties: {
            cid: { type: 'string', description: 'IPFS Content Identifier' },
            timeout: {
              type: 'number',
              default: 45000,
              description: 'Timeout in milliseconds',
            },
            response_format: {
              type: 'string',
              enum: ['json', 'markdown'],
              default: 'markdown',
            },
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
            response_format: {
              type: 'string',
              enum: ['json', 'markdown'],
              default: 'markdown',
            },
          },
        },
      },

      // Browser Tools
      {
        name: 'browser_session',
        description: 'Automate browser actions using Playwright or Puppeteer',
        inputSchema: {
          type: 'object',
          properties: {
            url: { type: 'string', description: 'Initial URL to navigate to' },
            actions: {
              type: 'array',
              items: {
                type: 'object',
                properties: {
                  type: {
                    type: 'string',
                    enum: ['navigate', 'click', 'type', 'screenshot', 'evaluate', 'waitFor', 'select', 'scroll'],
                  },
                  selector: { type: 'string' },
                  value: { type: 'string' },
                  url: { type: 'string' },
                  script: { type: 'string' },
                  timeout: { type: 'number', default: 10000 },
                },
                required: ['type'],
              },
              default: [],
              description: 'Sequence of browser actions',
            },
            headless: {
              type: 'boolean',
              default: true,
              description: 'Run browser headless',
            },
            engine: {
              type: 'string',
              enum: ['playwright', 'puppeteer'],
              default: 'playwright',
              description: 'Browser engine',
            },
            viewport: {
              type: 'object',
              properties: {
                width: { type: 'number', default: 1280 },
                height: { type: 'number', default: 720 },
              },
            },
            response_format: {
              type: 'string',
              enum: ['json', 'markdown'],
              default: 'markdown',
            },
          },
          required: ['url'],
        },
      },

      // DNS Tools
      {
        name: 'dyndns_update',
        description: 'Update dynamic DNS record (supports DuckDNS, NoIP, DynDNS, Cloudflare)',
        inputSchema: {
          type: 'object',
          properties: {
            hostname: {
              type: 'string',
              description: 'Hostname to update (e.g., squirrel.duckdns.org)',
            },
            ip: {
              type: 'string',
              description: 'IP address (auto-detected if not provided)',
            },
            provider: {
              type: 'string',
              enum: ['duckdns', 'noip', 'dyndns', 'cloudflare', 'custom'],
              default: 'duckdns',
              description: 'DNS provider',
            },
            token: { type: 'string', description: 'API token for the provider' },
            response_format: {
              type: 'string',
              enum: ['json', 'markdown'],
              default: 'markdown',
            },
          },
          required: ['hostname'],
        },
      },
      {
        name: 'dyndns_resolve',
        description: 'Resolve DNS records (A, AAAA, CNAME, MX, TXT, NS)',
        inputSchema: {
          type: 'object',
          properties: {
            hostname: { type: 'string', description: 'Hostname to resolve' },
            type: {
              type: 'string',
              enum: ['A', 'AAAA', 'CNAME', 'MX', 'TXT', 'NS'],
              default: 'A',
              description: 'DNS record type',
            },
            response_format: {
              type: 'string',
              enum: ['json', 'markdown'],
              default: 'markdown',
            },
          },
          required: ['hostname'],
        },
      },

      // Clipboard Tools
      {
        name: 'clipboard_write',
        description: 'Write content to clipboard buffer',
        inputSchema: {
          type: 'object',
          properties: {
            content: { type: 'string', description: 'Content to store' },
            format: {
              type: 'string',
              enum: ['text', 'html', 'json'],
              default: 'text',
              description: 'Content format',
            },
            label: { type: 'string', description: 'Optional label' },
          },
          required: ['content'],
        },
      },
      {
        name: 'clipboard_read',
        description: 'Read recent clipboard entries',
        inputSchema: {
          type: 'object',
          properties: {
            last: {
              type: 'number',
              default: 1,
              description: 'Number of recent entries to read',
            },
            format: {
              type: 'string',
              enum: ['text', 'html', 'json'],
              default: 'text',
              description: 'Output format',
            },
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
            limit: {
              type: 'number',
              default: 10,
              description: 'Maximum results',
            },
          },
          required: ['query'],
        },
      },

      // Pipeline Tools
      {
        name: 'nusskette_run',
        description: '🐿️ Run a NussKette (Nut Chain) pipeline — chain multiple tools together through HASEL monads',
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
                  monad: {
                    type: 'string',
                    enum: ['text', 'binary', 'mixed', 'url', 'ipfs', 'dom', 'dns', 'clipboard', 'io'],
                    description: 'Monad type',
                  },
                  action: { type: 'string', description: 'Tool name' },
                  params: {
                    type: 'object',
                    additionalProperties: true,
                    description: 'Tool parameters',
                  },
                },
                required: ['monad', 'action', 'params'],
              },
              description: 'Ordered pipeline steps',
            },
            response_format: {
              type: 'string',
              enum: ['json', 'markdown'],
              default: 'markdown',
            },
          },
          required: ['name', 'steps'],
        },
      },

      // System Tools
      {
        name: 'system_status',
        description: 'Get system status, server info, and available tools',
        inputSchema: {
          type: 'object',
          properties: {
            response_format: {
              type: 'string',
              enum: ['json', 'markdown'],
              default: 'markdown',
            },
          },
        },
      },
    ];
  }

  /**
   * Start the MCP server
   */
  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);

    console.error('🐿️ Eichhörnchen Master MCP Server started!');
    console.error(`Version: ${SERVER_VERSION}`);
    console.error(SERVER_DESCRIPTION);
    console.error('');
    console.error('Available tools:');
    console.error('  Web: web_fetch, web_scrape, web_crawl');
    console.error('  IPFS: ipfs_add, ipfs_get, ipfs_list_gateways');
    console.error('  Browser: browser_session');
    console.error('  DNS: dyndns_update, dyndns_resolve');
    console.error('  Clipboard: clipboard_write, clipboard_read, clipboard_search');
    console.error('  Pipeline: nusskette_run');
    console.error('  System: system_status');
    console.error('');
    console.error('🌰 HASEL Monads: text, binary, mixed, url, ipfs, dom, dns, clipboard, io');
    console.error('');
  }
}

// Start the server
const server = new EichhoernchenMCPServer();
server.run().catch((error) => {
  console.error('Failed to start server:', error);
  process.exit(1);
});

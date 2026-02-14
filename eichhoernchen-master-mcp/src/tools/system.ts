// 🐿️ System Tools — Status and Info

import type { ToolResult, ResponseFormat } from '../types.js';
import { formatResponse, successResult, errorResult } from '../shared.js';
import { SERVER_NAME, SERVER_VERSION, SERVER_DESCRIPTION } from '../constants.js';
import * as os from 'os';

export interface SystemStatusParams {
  response_format: ResponseFormat;
}

/**
 * Get system status and server information
 */
export async function systemStatus(params: SystemStatusParams): Promise<ToolResult> {
  try {
    const uptime = process.uptime();
    const memUsage = process.memoryUsage();

    const result = {
      server: {
        name: SERVER_NAME,
        version: SERVER_VERSION,
        description: SERVER_DESCRIPTION,
        uptime: `${Math.floor(uptime / 60)} minutes`,
        uptimeSeconds: uptime,
      },
      system: {
        platform: os.platform(),
        arch: os.arch(),
        nodeVersion: process.version,
        cpus: os.cpus().length,
        totalMemory: `${Math.round(os.totalmem() / 1024 / 1024)} MB`,
        freeMemory: `${Math.round(os.freemem() / 1024 / 1024)} MB`,
      },
      process: {
        pid: process.pid,
        memoryUsage: {
          rss: `${Math.round(memUsage.rss / 1024 / 1024)} MB`,
          heapTotal: `${Math.round(memUsage.heapTotal / 1024 / 1024)} MB`,
          heapUsed: `${Math.round(memUsage.heapUsed / 1024 / 1024)} MB`,
          external: `${Math.round(memUsage.external / 1024 / 1024)} MB`,
        },
      },
      tools: {
        web: ['web_fetch', 'web_scrape', 'web_crawl'],
        ipfs: ['ipfs_add', 'ipfs_get', 'ipfs_list_gateways'],
        browser: ['browser_session'],
        dns: ['dyndns_update', 'dyndns_resolve'],
        clipboard: ['clipboard_write', 'clipboard_read', 'clipboard_search'],
        pipeline: ['nusskette_run'],
        system: ['system_status'],
      },
      hasel: {
        monads: ['text', 'binary', 'mixed', 'url', 'ipfs', 'dom', 'dns', 'clipboard', 'io'],
        philosophy: 'Every operation flows through typed monads 🐿️',
      },
    };

    return successResult(formatResponse(result, params.response_format));
  } catch (error) {
    return errorResult(error as Error);
  }
}

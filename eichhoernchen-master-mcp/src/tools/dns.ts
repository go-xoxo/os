// 🐿️ DynDNS Tools — Update and Resolve

import type { ToolResult, ResponseFormat, DNSProvider, DNSRecordType } from '../types.js';
import { makeHttpRequest, formatResponse, successResult, errorResult, getPublicIP } from '../shared.js';
import { DNSMonad } from '../hasel.js';
import { DYNDNS_ENDPOINTS, DYNDNS_TOKEN } from '../constants.js';
import * as dns from 'dns/promises';

export interface DynDNSUpdateParams {
  hostname: string;
  ip?: string;
  provider: DNSProvider;
  token?: string;
  response_format: ResponseFormat;
}

export interface DynDNSResolveParams {
  hostname: string;
  type: DNSRecordType;
  response_format: ResponseFormat;
}

/**
 * Update dynamic DNS record
 */
export async function dyndnsUpdate(params: DynDNSUpdateParams): Promise<ToolResult> {
  try {
    const dnsMonad = new DNSMonad(params.hostname);
    if (!dnsMonad.isValidHostname()) {
      return errorResult('Invalid hostname format');
    }

    // Get IP if not provided
    const ip = params.ip || (await getPublicIP());

    // Get token
    const token = params.token || DYNDNS_TOKEN;
    if (!token && params.provider !== 'custom') {
      return errorResult('DNS provider token is required');
    }

    // Update based on provider
    const updateResult = await updateDNSProvider(params.provider, params.hostname, ip, token);

    const result = {
      hostname: params.hostname,
      ip,
      provider: params.provider,
      status: updateResult.success ? 'updated' : 'failed',
      message: updateResult.message,
    };

    return successResult(formatResponse(result, params.response_format), {
      ip,
      provider: params.provider,
    });
  } catch (error) {
    return errorResult(error as Error);
  }
}

async function updateDNSProvider(
  provider: DNSProvider,
  hostname: string,
  ip: string,
  token: string
): Promise<{ success: boolean; message: string }> {
  try {
    const endpoint = DYNDNS_ENDPOINTS[provider];
    if (!endpoint) {
      return { success: false, message: 'Unknown provider' };
    }

    let url: string;
    let config: Record<string, unknown> = {};

    switch (provider) {
      case 'duckdns': {
        const domain = hostname.replace('.duckdns.org', '');
        url = `${endpoint}?domains=${domain}&token=${token}&ip=${ip}`;
        break;
      }

      case 'noip':
      case 'dyndns': {
        url = `${endpoint}?hostname=${hostname}&myip=${ip}`;
        config = {
          auth: {
            username: token.split(':')[0] || '',
            password: token.split(':')[1] || '',
          },
        };
        break;
      }

      case 'cloudflare': {
        return {
          success: false,
          message: 'Cloudflare implementation requires zone ID and record ID',
        };
      }

      case 'custom': {
        url = endpoint.replace('{hostname}', hostname).replace('{ip}', ip).replace('{token}', token);
        break;
      }

      default:
        return { success: false, message: 'Unsupported provider' };
    }

    const response = await makeHttpRequest(url, config);

    // DuckDNS returns "OK" or "KO"
    if (provider === 'duckdns') {
      const success = String(response.data).trim() === 'OK';
      return {
        success,
        message: success ? 'DNS updated successfully' : 'DNS update failed',
      };
    }

    // Most providers return success status
    return {
      success: response.status >= 200 && response.status < 300,
      message: `HTTP ${response.status}`,
    };
  } catch (error) {
    return {
      success: false,
      message: error instanceof Error ? error.message : String(error),
    };
  }
}

/**
 * Resolve DNS records
 */
export async function dyndnsResolve(params: DynDNSResolveParams): Promise<ToolResult> {
  try {
    const dnsMonad = new DNSMonad(params.hostname);
    if (!dnsMonad.isValidHostname()) {
      return errorResult('Invalid hostname format');
    }

    let records: string[] = [];

    switch (params.type) {
      case 'A':
        records = await dns.resolve4(params.hostname);
        break;
      case 'AAAA':
        records = await dns.resolve6(params.hostname);
        break;
      case 'CNAME':
        records = await dns.resolveCname(params.hostname);
        break;
      case 'MX': {
        const mxRecords = await dns.resolveMx(params.hostname);
        records = mxRecords.map((r) => `${r.priority} ${r.exchange}`);
        break;
      }
      case 'TXT':
        const txtRecords = await dns.resolveTxt(params.hostname);
        records = txtRecords.map((r) => r.join(' '));
        break;
      case 'NS':
        records = await dns.resolveNs(params.hostname);
        break;
      default:
        return errorResult(`Unsupported DNS record type: ${params.type}`);
    }

    const result = {
      hostname: params.hostname,
      type: params.type,
      records,
      count: records.length,
    };

    return successResult(formatResponse(result, params.response_format), {
      recordCount: records.length,
    });
  } catch (error) {
    return errorResult(error as Error);
  }
}

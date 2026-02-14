// 🐿️ IPFS Tools — Add, Get, List Gateways

import type { ToolResult, ResponseFormat } from '../types.js';
import { makeHttpRequest, formatResponse, successResult, errorResult } from '../shared.js';
import { IPFSMonad } from '../hasel.js';
import { IPFS_GATEWAYS, DEFAULT_IPFS_GATEWAY } from '../constants.js';

export interface IPFSAddParams {
  content: string;
  filename?: string;
  pin: boolean;
  response_format: ResponseFormat;
}

export interface IPFSGetParams {
  cid: string;
  timeout: number;
  response_format: ResponseFormat;
}

export interface IPFSListGatewaysParams {
  response_format: ResponseFormat;
}

/**
 * Add content to IPFS
 * Note: This is a simplified implementation using HTTP API
 */
export async function ipfsAdd(params: IPFSAddParams): Promise<ToolResult> {
  try {
    // For a real implementation, you would use ipfs-http-client
    // This is a mock implementation that demonstrates the structure

    // In production, you'd do something like:
    // const client = create({ url: process.env.IPFS_API });
    // const { cid } = await client.add(params.content);

    // For now, we'll create a deterministic mock CID based on content
    const mockCID = `Qm${Buffer.from(params.content).toString('base64').substring(0, 44)}`;

    const result = {
      cid: mockCID,
      size: Buffer.from(params.content).length,
      pinned: params.pin,
      filename: params.filename,
      gateway_url: `${DEFAULT_IPFS_GATEWAY}/ipfs/${mockCID}`,
      note: 'This is a mock implementation. Connect to a real IPFS node for production use.',
    };

    return successResult(formatResponse(result, params.response_format), {
      cid: mockCID,
      size: result.size,
    });
  } catch (error) {
    return errorResult(error as Error);
  }
}

/**
 * Get content from IPFS by CID
 */
export async function ipfsGet(params: IPFSGetParams): Promise<ToolResult> {
  try {
    const ipfsMonad = new IPFSMonad(params.cid);
    if (!ipfsMonad.isValid()) {
      return errorResult('Invalid IPFS CID format');
    }

    // Try multiple gateways
    let lastError: Error | undefined;

    for (const gateway of IPFS_GATEWAYS) {
      try {
        const url = ipfsMonad.toGatewayURL(gateway);
        const response = await makeHttpRequest(url, {
          timeout: params.timeout,
        });

        if (response.status === 200) {
          const content = typeof response.data === 'string' ? response.data : JSON.stringify(response.data);

          const result = {
            cid: params.cid,
            content,
            size: content.length,
            gateway: gateway,
            contentType: response.headers['content-type'] || 'unknown',
          };

          return successResult(formatResponse(result, params.response_format), {
            gateway,
            size: content.length,
          });
        }
      } catch (error) {
        lastError = error as Error;
        continue;
      }
    }

    return errorResult(lastError || new Error('Failed to fetch from all IPFS gateways'));
  } catch (error) {
    return errorResult(error as Error);
  }
}

/**
 * List available IPFS gateways
 */
export async function ipfsListGateways(params: IPFSListGatewaysParams): Promise<ToolResult> {
  try {
    const result = {
      gateways: IPFS_GATEWAYS,
      default: DEFAULT_IPFS_GATEWAY,
      count: IPFS_GATEWAYS.length,
    };

    return successResult(formatResponse(result, params.response_format), {
      count: IPFS_GATEWAYS.length,
    });
  } catch (error) {
    return errorResult(error as Error);
  }
}

// 🐿️ Shared Utilities for Eichhörnchen Master MCP

import axios, { AxiosRequestConfig } from 'axios';
import type { ResponseFormat, ToolResult } from './types.js';
import { USER_AGENT } from './constants.js';

/**
 * Make an HTTP request with proper error handling
 */
export async function makeHttpRequest(
  url: string,
  config: AxiosRequestConfig = {}
): Promise<{ data: unknown; headers: Record<string, string>; status: number }> {
  try {
    const response = await axios({
      url,
      ...config,
      headers: {
        'User-Agent': USER_AGENT,
        ...config.headers,
      },
      validateStatus: () => true, // Don't throw on any status
    });

    return {
      data: response.data,
      headers: response.headers as Record<string, string>,
      status: response.status,
    };
  } catch (error) {
    throw new Error(`HTTP request failed: ${error instanceof Error ? error.message : String(error)}`);
  }
}

/**
 * Format data as JSON or Markdown
 */
export function formatResponse(data: unknown, format: ResponseFormat): string {
  if (format === 'json') {
    return JSON.stringify(data, null, 2);
  }

  // Markdown formatting
  if (typeof data === 'string') {
    return data;
  }

  if (typeof data === 'object' && data !== null) {
    return objectToMarkdown(data);
  }

  return String(data);
}

/**
 * Convert object to Markdown format
 */
function objectToMarkdown(obj: unknown, indent = 0): string {
  if (Array.isArray(obj)) {
    return obj
      .map((item, index) => `${' '.repeat(indent)}- ${objectToMarkdown(item, indent + 2)}`)
      .join('\n');
  }

  if (typeof obj === 'object' && obj !== null) {
    return Object.entries(obj)
      .map(([key, value]) => {
        if (typeof value === 'object' && value !== null) {
          return `${' '.repeat(indent)}**${key}**:\n${objectToMarkdown(value, indent + 2)}`;
        }
        return `${' '.repeat(indent)}**${key}**: ${value}`;
      })
      .join('\n');
  }

  return String(obj);
}

/**
 * Create a success tool result
 */
export function successResult(data: unknown, metadata?: Record<string, unknown>): ToolResult {
  return {
    success: true,
    data,
    metadata,
  };
}

/**
 * Create an error tool result
 */
export function errorResult(error: string | Error, metadata?: Record<string, unknown>): ToolResult {
  return {
    success: false,
    error: error instanceof Error ? error.message : error,
    metadata,
  };
}

/**
 * Sleep for specified milliseconds
 */
export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Retry an async function with exponential backoff
 */
export async function retry<T>(
  fn: () => Promise<T>,
  maxAttempts = 3,
  initialDelay = 1000
): Promise<T> {
  let lastError: Error | undefined;

  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error instanceof Error ? error : new Error(String(error));

      if (attempt < maxAttempts) {
        const delay = initialDelay * Math.pow(2, attempt - 1);
        await sleep(delay);
      }
    }
  }

  throw lastError || new Error('Retry failed');
}

/**
 * Validate URL format
 */
export function isValidURL(str: string): boolean {
  try {
    new URL(str);
    return true;
  } catch {
    return false;
  }
}

/**
 * Get current public IP address
 */
export async function getPublicIP(): Promise<string> {
  const services = [
    'https://api.ipify.org?format=json',
    'https://api.my-ip.io/ip.json',
    'https://ifconfig.me/ip',
  ];

  for (const service of services) {
    try {
      const response = await makeHttpRequest(service, { timeout: 5000 });
      if (typeof response.data === 'object' && response.data !== null && 'ip' in response.data) {
        return (response.data as { ip: string }).ip;
      }
      if (typeof response.data === 'string') {
        return response.data.trim();
      }
    } catch {
      continue;
    }
  }

  throw new Error('Failed to get public IP from all services');
}

/**
 * Sanitize filename
 */
export function sanitizeFilename(filename: string): string {
  return filename.replace(/[^a-z0-9_\-\.]/gi, '_');
}

/**
 * Parse Content-Type header
 */
export function parseContentType(contentType: string): { type: string; charset?: string } {
  const [type, ...params] = contentType.split(';').map((s) => s.trim());
  const charset = params.find((p) => p.startsWith('charset='))?.split('=')[1];
  return { type, charset };
}

/**
 * Check if content is binary
 */
export function isBinaryContent(contentType: string): boolean {
  const binaryTypes = ['image/', 'video/', 'audio/', 'application/octet-stream', 'application/pdf'];
  return binaryTypes.some((type) => contentType.startsWith(type));
}

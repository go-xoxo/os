/**
 * 🐿️ Shared Utilities for HASEL MCP Server
 */

import { Monads, NussMonad } from './hasel.js';
import { ToolResult } from './types.js';
import { ERROR_MESSAGES, MARKDOWN_TEMPLATES } from './constants.js';

// ═══════════════════════════════════════════════════
// 🔧 Error Handling
// ═══════════════════════════════════════════════════

export class HASELError extends Error {
  constructor(
    message: string,
    public code: string,
    public details?: unknown
  ) {
    super(message);
    this.name = 'HASELError';
  }
}

export function createError(code: keyof typeof ERROR_MESSAGES, details?: unknown): HASELError {
  return new HASELError(ERROR_MESSAGES[code], code, details);
}

export function handleError(error: unknown): ToolResult {
  if (error instanceof HASELError) {
    return {
      success: false,
      error: error.message,
      metadata: {
        code: error.code,
        details: error.details,
      },
    };
  }

  if (error instanceof Error) {
    return {
      success: false,
      error: error.message,
      metadata: {
        code: 'UNKNOWN_ERROR',
        stack: error.stack,
      },
    };
  }

  return {
    success: false,
    error: String(error),
    metadata: {
      code: 'UNKNOWN_ERROR',
    },
  };
}

// ═══════════════════════════════════════════════════
// 📊 Response Formatting
// ═══════════════════════════════════════════════════

export function formatAsJSON(data: unknown): string {
  return JSON.stringify(data, null, 2);
}

export function formatAsMarkdown(title: string, content: string, type: 'success' | 'error' | 'info' | 'warning' = 'success'): string {
  return MARKDOWN_TEMPLATES[type](title, content);
}

export function formatToolResult(result: ToolResult, format: 'json' | 'markdown' = 'markdown', title?: string): string {
  if (format === 'json') {
    return formatAsJSON(result);
  }

  if (!result.success) {
    return formatAsMarkdown(title || 'Error', result.error || 'Unknown error', 'error');
  }

  const content = typeof result.data === 'string'
    ? result.data
    : formatAsJSON(result.data);

  return formatAsMarkdown(title || 'Success', content, 'success');
}

// ═══════════════════════════════════════════════════
// 🌐 HTTP Utilities
// ═══════════════════════════════════════════════════

export async function fetchWithTimeout(
  url: string,
  options: RequestInit & { timeout?: number } = {}
): Promise<Response> {
  const { timeout = 30000, ...fetchOptions } = options;

  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), timeout);

  try {
    const response = await fetch(url, {
      ...fetchOptions,
      signal: controller.signal,
    });
    clearTimeout(timeoutId);
    return response;
  } catch (error) {
    clearTimeout(timeoutId);
    if ((error as Error).name === 'AbortError') {
      throw createError('TIMEOUT', { url, timeout });
    }
    throw createError('NETWORK_ERROR', { url, error });
  }
}

export function isValidUrl(url: string): boolean {
  try {
    new URL(url);
    return true;
  } catch {
    return false;
  }
}

// ═══════════════════════════════════════════════════
// 🎯 Data Validation
// ═══════════════════════════════════════════════════

export function validateUrl(url: string): void {
  if (!isValidUrl(url)) {
    throw createError('INVALID_URL', { url });
  }
}

export function validateTimeout(timeout: number, max: number): void {
  if (timeout < 1000 || timeout > max) {
    throw createError('INVALID_PARAMS', {
      message: `Timeout must be between 1000ms and ${max}ms`,
      timeout,
      max,
    });
  }
}

// ═══════════════════════════════════════════════════
// 📋 Clipboard History (In-Memory)
// ═══════════════════════════════════════════════════

import { ClipboardEntry } from './types.js';

export class ClipboardHistory {
  private history: ClipboardEntry[] = [];
  private maxSize: number;

  constructor(maxSize: number = 100) {
    this.maxSize = maxSize;
  }

  add(entry: ClipboardEntry): void {
    this.history.unshift(entry);
    if (this.history.length > this.maxSize) {
      this.history = this.history.slice(0, this.maxSize);
    }
  }

  getLast(count: number = 1): ClipboardEntry[] {
    return this.history.slice(0, Math.min(count, this.history.length));
  }

  search(query: string, limit: number = 10): ClipboardEntry[] {
    const lowerQuery = query.toLowerCase();
    return this.history
      .filter(entry =>
        entry.content.toLowerCase().includes(lowerQuery) ||
        entry.label?.toLowerCase().includes(lowerQuery)
      )
      .slice(0, limit);
  }

  clear(): void {
    this.history = [];
  }

  size(): number {
    return this.history.length;
  }
}

// Global clipboard history instance
export const clipboardHistory = new ClipboardHistory();

// ═══════════════════════════════════════════════════
// 🔄 Retry Logic
// ═══════════════════════════════════════════════════

export async function retry<T>(
  fn: () => Promise<T>,
  maxAttempts: number = 3,
  delayMs: number = 1000
): Promise<T> {
  let lastError: unknown;

  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error;
      if (attempt < maxAttempts) {
        await new Promise(resolve => setTimeout(resolve, delayMs * attempt));
      }
    }
  }

  throw lastError;
}

// ═══════════════════════════════════════════════════
// 🕐 Utilities
// ═══════════════════════════════════════════════════

export function sleep(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}

export function getCurrentTimestamp(): number {
  return Date.now();
}

export function formatTimestamp(timestamp: number): string {
  return new Date(timestamp).toISOString();
}

// ═══════════════════════════════════════════════════
// 🎨 Monad Utilities
// ═══════════════════════════════════════════════════

export function wrapInMonad<T>(data: T, type: 'text' | 'mixed' = 'mixed'): NussMonad<T> {
  return type === 'text' && typeof data === 'string'
    ? Monads.text(data) as any as NussMonad<T>
    : Monads.mixed(data) as any as NussMonad<T>;
}

export async function executeWithMonad<T>(
  fn: () => Promise<T>,
  monadType: 'text' | 'mixed' = 'mixed'
): Promise<NussMonad<T>> {
  try {
    const result = await fn();
    return wrapInMonad(result, monadType);
  } catch (error) {
    return Monads.error(error) as any as NussMonad<T>;
  }
}

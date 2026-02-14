// 🐿️ Clipboard Tools — Write, Read, Search

import type { ToolResult, ResponseFormat, ClipboardFormat, ClipboardEntry } from '../types.js';
import { formatResponse, successResult, errorResult } from '../shared.js';
import { ClipboardMonad } from '../hasel.js';
import { CLIPBOARD_MAX_HISTORY } from '../constants.js';

// In-memory clipboard history
const clipboardHistory: ClipboardEntry[] = [];

export interface ClipboardWriteParams {
  content: string;
  format: ClipboardFormat;
  label?: string;
}

export interface ClipboardReadParams {
  last: number;
  format: ClipboardFormat;
}

export interface ClipboardSearchParams {
  query: string;
  limit: number;
}

/**
 * Write content to clipboard
 */
export async function clipboardWrite(params: ClipboardWriteParams): Promise<ToolResult> {
  try {
    const entry: ClipboardEntry = {
      content: params.content,
      format: params.format,
      timestamp: Date.now(),
      label: params.label,
    };

    // Add to history
    clipboardHistory.unshift(entry);

    // Keep only max history
    if (clipboardHistory.length > CLIPBOARD_MAX_HISTORY) {
      clipboardHistory.length = CLIPBOARD_MAX_HISTORY;
    }

    // Try to write to actual system clipboard
    try {
      // This would use clipboardy in production
      // await clipboardy.write(params.content);
    } catch {
      // System clipboard not available, but we still have history
    }

    const result = {
      content: params.content.substring(0, 100) + (params.content.length > 100 ? '...' : ''),
      format: params.format,
      label: params.label,
      timestamp: entry.timestamp,
      historySize: clipboardHistory.length,
    };

    return successResult(formatResponse(result, 'markdown'), {
      historySize: clipboardHistory.length,
    });
  } catch (error) {
    return errorResult(error as Error);
  }
}

/**
 * Read recent clipboard entries
 */
export async function clipboardRead(params: ClipboardReadParams): Promise<ToolResult> {
  try {
    const entries = clipboardHistory.slice(0, params.last);

    const result = {
      count: entries.length,
      entries: entries.map((e) => ({
        content: e.content.substring(0, 200) + (e.content.length > 200 ? '...' : ''),
        format: e.format,
        label: e.label,
        timestamp: new Date(e.timestamp).toISOString(),
      })),
    };

    return successResult(formatResponse(result, 'markdown'), {
      count: entries.length,
    });
  } catch (error) {
    return errorResult(error as Error);
  }
}

/**
 * Search clipboard history
 */
export async function clipboardSearch(params: ClipboardSearchParams): Promise<ToolResult> {
  try {
    const query = params.query.toLowerCase();
    const matches = clipboardHistory
      .filter((e) => {
        const searchText = `${e.content} ${e.label || ''}`.toLowerCase();
        return searchText.includes(query);
      })
      .slice(0, params.limit);

    const result = {
      query: params.query,
      matches: matches.length,
      entries: matches.map((e) => ({
        content: e.content.substring(0, 200) + (e.content.length > 200 ? '...' : ''),
        format: e.format,
        label: e.label,
        timestamp: new Date(e.timestamp).toISOString(),
      })),
    };

    return successResult(formatResponse(result, 'markdown'), {
      matches: matches.length,
    });
  } catch (error) {
    return errorResult(error as Error);
  }
}

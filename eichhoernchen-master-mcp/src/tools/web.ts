// 🐿️ Web Tools — Fetch, Scrape, Crawl

import { load } from 'cheerio';
import type { ToolResult, ResponseFormat } from '../types.js';
import { makeHttpRequest, formatResponse, successResult, errorResult, isValidURL } from '../shared.js';
import { URLMonad } from '../hasel.js';

export interface WebFetchParams {
  url: string;
  method: 'GET' | 'POST' | 'PUT' | 'DELETE';
  headers?: Record<string, string>;
  body?: string;
  timeout: number;
  response_format: ResponseFormat;
}

export interface WebScrapeParams {
  url: string;
  selector?: string;
  waitFor?: string;
  screenshot: boolean;
  javascript: boolean;
  engine: 'cheerio' | 'playwright' | 'puppeteer';
  response_format: ResponseFormat;
}

export interface WebCrawlParams {
  url: string;
  depth: number;
  maxPages: number;
  pattern?: string;
  extractLinks: boolean;
  response_format: ResponseFormat;
}

/**
 * Fetch content from a URL
 */
export async function webFetch(params: WebFetchParams): Promise<ToolResult> {
  try {
    const urlMonad = new URLMonad(params.url);
    if (!urlMonad.isValid()) {
      return errorResult('Invalid URL format');
    }

    const response = await makeHttpRequest(params.url, {
      method: params.method,
      headers: params.headers,
      data: params.body,
      timeout: params.timeout,
    });

    const result = {
      status: response.status,
      headers: response.headers,
      body: response.data,
      url: params.url,
    };

    return successResult(formatResponse(result, params.response_format), {
      status: response.status,
      size: typeof response.data === 'string' ? response.data.length : 0,
    });
  } catch (error) {
    return errorResult(error as Error);
  }
}

/**
 * Scrape content from a web page
 */
export async function webScrape(params: WebScrapeParams): Promise<ToolResult> {
  try {
    if (!isValidURL(params.url)) {
      return errorResult('Invalid URL format');
    }

    // For now, we only implement cheerio (static scraping)
    // Playwright and Puppeteer would be imported dynamically
    if (params.engine === 'cheerio') {
      return await scrapeWithCheerio(params);
    }

    // Dynamic scraping would require browser automation
    return errorResult(`Engine ${params.engine} requires browser automation (not yet implemented in this simplified version)`);
  } catch (error) {
    return errorResult(error as Error);
  }
}

async function scrapeWithCheerio(params: WebScrapeParams): Promise<ToolResult> {
  try {
    const response = await makeHttpRequest(params.url);

    if (response.status !== 200) {
      return errorResult(`HTTP ${response.status}: Failed to fetch page`);
    }

    const html = typeof response.data === 'string' ? response.data : String(response.data);
    const $ = load(html);

    const selector = params.selector || 'body';
    const content = $(selector).text().trim();

    const links: string[] = [];
    $('a[href]').each((_, el) => {
      const href = $(el).attr('href');
      if (href) {
        try {
          const absoluteURL = new URL(href, params.url).href;
          links.push(absoluteURL);
        } catch {
          // Invalid URL, skip
        }
      }
    });

    const result = {
      url: params.url,
      content,
      links: [...new Set(links)], // Remove duplicates
      selector,
    };

    return successResult(formatResponse(result, params.response_format), {
      contentLength: content.length,
      linkCount: links.length,
    });
  } catch (error) {
    return errorResult(error as Error);
  }
}

/**
 * Crawl a website recursively
 */
export async function webCrawl(params: WebCrawlParams): Promise<ToolResult> {
  try {
    if (!isValidURL(params.url)) {
      return errorResult('Invalid URL format');
    }

    const visited = new Set<string>();
    const toVisit: Array<{ url: string; depth: number }> = [{ url: params.url, depth: 0 }];
    const results: Array<{ url: string; depth: number; title?: string; links: string[] }> = [];

    const urlPattern = params.pattern ? new RegExp(params.pattern) : null;

    while (toVisit.length > 0 && results.length < params.maxPages) {
      const current = toVisit.shift();
      if (!current || visited.has(current.url)) continue;

      // Check pattern match
      if (urlPattern && !urlPattern.test(current.url)) {
        continue;
      }

      visited.add(current.url);

      try {
        const response = await makeHttpRequest(current.url, { timeout: 10000 });
        if (response.status !== 200) continue;

        const html = typeof response.data === 'string' ? response.data : String(response.data);
        const $ = load(html);

        const title = $('title').text().trim();
        const links: string[] = [];

        // Extract links if we haven't reached max depth
        if (current.depth < params.depth) {
          $('a[href]').each((_, el) => {
            const href = $(el).attr('href');
            if (href) {
              try {
                const absoluteURL = new URL(href, current.url).href;
                // Only crawl same domain
                if (new URL(absoluteURL).hostname === new URL(params.url).hostname) {
                  links.push(absoluteURL);
                  if (!visited.has(absoluteURL) && toVisit.length < params.maxPages) {
                    toVisit.push({ url: absoluteURL, depth: current.depth + 1 });
                  }
                }
              } catch {
                // Invalid URL, skip
              }
            }
          });
        }

        results.push({
          url: current.url,
          depth: current.depth,
          title,
          links: [...new Set(links)],
        });
      } catch {
        // Failed to fetch, skip
        continue;
      }
    }

    const result = {
      startUrl: params.url,
      pagesVisited: results.length,
      maxDepth: params.depth,
      pages: results,
    };

    return successResult(formatResponse(result, params.response_format), {
      pagesVisited: results.length,
    });
  } catch (error) {
    return errorResult(error as Error);
  }
}

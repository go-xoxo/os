// 🐿️ Constants for Eichhörnchen Master MCP

export const SERVER_NAME = 'eichhoernchen-master-mcp';
export const SERVER_VERSION = '1.0.0';
export const SERVER_DESCRIPTION = '🐿️ Master MCP Server — HASEL Monads | Web | IPFS | DynDNS | Browser';

// IPFS Configuration
export const DEFAULT_IPFS_GATEWAY = process.env.IPFS_GATEWAY || 'https://ipfs.io';
export const DEFAULT_IPFS_API = process.env.IPFS_API || 'https://ipfs.io';

export const IPFS_GATEWAYS = [
  'https://ipfs.io',
  'https://dweb.link',
  'https://cloudflare-ipfs.com',
  'https://gateway.pinata.cloud',
  'https://ipfs.filebase.io',
];

// DynDNS Configuration
export const DEFAULT_DYNDNS_PROVIDER = process.env.DYNDNS_PROVIDER || 'duckdns';
export const DYNDNS_TOKEN = process.env.DYNDNS_TOKEN || '';

export const DYNDNS_ENDPOINTS = {
  duckdns: 'https://www.duckdns.org/update',
  noip: 'https://dynupdate.no-ip.com/nic/update',
  dyndns: 'https://members.dyndns.org/nic/update',
  cloudflare: 'https://api.cloudflare.com/client/v4',
  custom: process.env.DYNDNS_CUSTOM_ENDPOINT || '',
};

// Browser Configuration
export const DEFAULT_BROWSER_TIMEOUT = 10000;
export const DEFAULT_BROWSER_VIEWPORT = {
  width: 1280,
  height: 720,
};

// Web Configuration
export const DEFAULT_WEB_TIMEOUT = 30000;
export const MAX_WEB_TIMEOUT = 120000;

// Clipboard Configuration
export const CLIPBOARD_MAX_HISTORY = 100;

// User Agent
export const USER_AGENT = 'Eichhoernchen-MCP/1.0.0 (Squirrel OS; +https://github.com/squirrel-os)';

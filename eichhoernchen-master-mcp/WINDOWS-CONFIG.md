# 🐿️ Windows Configuration Guide

## For Windows Users

To use this MCP server with Claude Desktop on Windows, follow these steps:

### 1. Build the Project

First, navigate to the project directory and build it:

```cmd
cd C:\path\to\eichhoernchen-master-mcp
npm install
npm run build
```

### 2. Configure Claude Desktop

The Claude Desktop configuration file on Windows is located at:

```
C:\Users\frits\AppData\Roaming\Claude\claude_desktop_config.json
```

Open this file in a text editor and add the server configuration:

```json
{
  "mcpServers": {
    "eichhoernchen-master": {
      "command": "node",
      "args": [
        "C:\\Users\\frits\\path\\to\\eichhoernchen-master-mcp\\dist\\index.js"
      ],
      "env": {
        "IPFS_GATEWAY": "https://ipfs.io",
        "IPFS_API": "https://ipfs.io",
        "DYNDNS_PROVIDER": "duckdns",
        "DYNDNS_TOKEN": "your-duckdns-token-here"
      }
    }
  }
}
```

**Important Notes:**
- Use double backslashes (`\\`) in Windows paths
- Replace `C:\\Users\\frits\\path\\to\\` with the actual path to your project
- If you have other MCP servers, add this as another entry in the `mcpServers` object

### 3. Environment Variables

Configure these environment variables based on your needs:

- `IPFS_GATEWAY`: Your preferred IPFS gateway (default: https://ipfs.io)
- `IPFS_API`: Your IPFS API endpoint (for adding content)
- `DYNDNS_PROVIDER`: Your DNS provider (`duckdns`, `noip`, `cloudflare`, etc.)
- `DYNDNS_TOKEN`: Your DNS provider API token

#### For DuckDNS:
Get your token from: https://www.duckdns.org/

#### For NoIP:
Use format: `username:password`

#### For Cloudflare:
Requires API token from: https://dash.cloudflare.com/profile/api-tokens

### 4. Restart Claude Desktop

After saving the configuration:
1. Close Claude Desktop completely
2. Reopen it
3. The Eichhörnchen Master MCP Server should now be available!

### 5. Verify Installation

In Claude Desktop, try asking:

```
Can you check the system status of the Eichhörnchen MCP server?
```

Claude should respond with information about the server, available tools, and HASEL monads.

## Example Usage

### Update DynDNS
```
Update my DynDNS hostname squirrel.duckdns.org
```

### Scrape a Website
```
Scrape https://example.com and extract all links
```

### Create a Pipeline
```
Create a NussKette pipeline that:
1. Fetches content from a URL
2. Adds it to IPFS
3. Updates my DNS to point to the IPFS content
```

## Troubleshooting

### "Command not found" error
- Make sure Node.js is installed: `node --version`
- Ensure the path to index.js is correct

### "Module not found" error
- Run `npm install` in the project directory
- Run `npm run build` to compile TypeScript

### IPFS errors
- Check your IPFS gateway is accessible
- Try alternative gateways: cloudflare-ipfs.com, dweb.link

### DynDNS errors
- Verify your token is correct
- Check the hostname format matches your provider

## Advanced: Network Access

To make your MCP server available over the internet:

1. Set up port forwarding on your router
2. Use your DynDNS hostname for external access
3. Consider using IPFS for distributed content
4. Set up proper authentication and HTTPS

---

**Built with 🌰 by the Squirrel OS Team**

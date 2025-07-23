# =================================================================
# Installation Script (install.sh)
# =================================================================

#!/bin/bash 
set -e

echo "Installing MCP Common Lisp Server..."

# Check for Lisp implementation
if command -v sbcl >/dev/null 2>&1; then
    LISP=sbcl
elif command -v ccl >/dev/null 2>&1; then
    LISP=ccl
elif command -v ecl >/dev/null 2>&1; then
    LISP=ecl
else
    echo "Error: No supported Lisp implementation found"
    echo "Please install SBCL, CCL, or ECL"
    exit 1
fi

echo "Using Lisp implementation: $LISP"

# Check for Quicklisp
QUICKLISP_INIT="$HOME/quicklisp/setup.lisp"
if [ ! -f "$QUICKLISP_INIT" ]; then
    echo "Quicklisp not found. Installing..."
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    $LISP --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
    rm quicklisp.lisp
fi

# Build the server
echo "Building MCP server..."
make LISP=$LISP build

# Install system-wide
echo "Installing to /usr/local/bin..."
sudo cp mcp-server /usr/local/bin/
sudo chmod +x /usr/local/bin/mcp-server

# Setup Claude Desktop config if directory exists
CLAUDE_CONFIG="$HOME/Library/Application Support/Claude/claude_desktop_config.json"
if [ -d "$(dirname "$CLAUDE_CONFIG")" ]; then
    echo "Updating Claude Desktop configuration..."
    if [ -f "$CLAUDE_CONFIG" ]; then
        # Backup existing config
        cp "$CLAUDE_CONFIG" "$CLAUDE_CONFIG.backup"
    fi
    
    # Update or create config
    python3 - <<EOF
import json
import os

config_path = "$CLAUDE_CONFIG"
config = {}

if os.path.exists(config_path):
    with open(config_path, 'r') as f:
        config = json.load(f)

if 'mcpServers' not in config:
    config['mcpServers'] = {}

config['mcpServers']['lisp-mcp'] = {
    "command": "/usr/local/bin/mcp-server",
    "args": ["--mcp"]
}

with open(config_path, 'w') as f:
    json.dump(config, f, indent=2)
EOF
    
    echo "Claude Desktop configuration updated"
fi

echo "Installation complete!"
echo "You can now run: mcp-server --help"

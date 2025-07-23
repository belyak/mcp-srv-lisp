# MCP Common Lisp Server Template

This is a port of the Rust MCP (Model Context Protocol) server template to Common Lisp.

## Project Structure

```
mcp-lisp-server/
├── mcp-server.asd              # ASDF system definition
├── README.md                   # This file
├── LICENSE                     # Apache 2.0 license
├── build.lisp                  # Build script
├── install.sh                  # Installation script
├── Makefile                    # Build automation
├── src/
│   ├── package.lisp            # Package definitions
│   ├── constants.lisp          # Constants and configuration
│   ├── types.lisp              # Type definitions and JSON handling
│   ├── templates.lisp          # Template data (tools, prompts, resources)
│   ├── utilities.lisp          # Core utilities and MCP handlers
│   ├── tools.lisp              # Tools implementation
│   ├── prompts.lisp            # Prompts implementation
│   ├── resources.lisp          # Resources implementation
│   └── main.lisp               # Main server and CLI
└── tests/
    ├── test-package.lisp       # Test package definition
    ├── test-types.lisp         # Type system tests
    ├── test-tools.lisp         # Tools tests
    ├── test-prompts.lisp       # Prompts tests
    ├── test-resources.lisp     # Resources tests
    └── test-utilities.lisp     # Utilities tests
```

## Dependencies

The server requires the following Common Lisp libraries:

- **alexandria** - General utilities
- **serapeum** - Additional utilities
- **yason** - JSON parsing and encoding
- **bordeaux-threads** - Threading support
- **usocket** - Socket operations
- **flexi-streams** - Flexible stream operations
- **local-time** - Time handling
- **quri** - URI parsing
- **cl-ppcre** - Regular expressions
- **unix-opts** - Command line argument parsing
- **trivial-signal** - Signal handling

## Installation

### Prerequisites

1. Install SBCL (Steel Bank Common Lisp) or another supported Lisp implementation
2. Install Quicklisp for dependency management

### Quick Installation

```bash
# Clone the repository
git clone https://github.com/your-username/mcp-lisp-server.git
cd mcp-lisp-server

# Install dependencies and build
make install

# Or manually:
sbcl --load build.lisp
```

### Manual Installation

```lisp
;; In SBCL REPL:
(ql:quickload :mcp-server)
(asdf:make :mcp-server)
```

## Usage

### Command Line Interface

```bash
# List available tools
./mcp-server --tools

# List prompts and resources
./mcp-server --prompts --resources

# JSON output
./mcp-server --tools --json

# Start MCP server
./mcp-server --mcp
# or
./mcp-server --stdio
```

### Integration with Claude Desktop

Add to your `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "lisp-server": {
      "command": "/path/to/mcp-server",
      "args": ["--mcp"]
    }
  }
}
```

## Features

### Core MCP Protocol Support

- **Full JSON-RPC Implementation** - Complete MCP protocol support
- **Tools** - Example time tool with extensible framework
- **Prompts** - Template system for dynamic prompt generation
- **Resources** - Resource management with subscription support
- **Signal Handling** - Graceful shutdown with SIGINT/SIGTERM
- **Logging** - Built-in logging to `/tmp/mcp.jsonl`

### Available Tools

- `get_current_time_in_city` - Get current time for a specified city

### Available Prompts

- `current-time` - Display current time in a city
- `analyze-code` - Analyze code for potential improvements

### Available Resources

- `Application Logs` - Sample log file resource at `file:///logs/app.log`

## Development

### Building from Source

```bash
# Using Make
make build

# Using SBCL directly
sbcl --load build.lisp

# Using ASDF
sbcl --eval "(asdf:make :mcp-server)" --quit
```

### Running Tests

```bash
# Run all tests
make test

# Or in SBCL
sbcl --eval "(asdf:test-system :mcp-server)" --quit
```

### Development REPL

```lisp
;; Load the system for development
(ql:quickload :mcp-server)

;; Start server programmatically
(mcp-server.main:start-server)

;; Test individual components
(mcp-server.tools:handle-tools-list nil)
(mcp-server.prompts:handle-prompts-list nil)
```

## Customization

### Adding New Tools

1. Add tool definition to `templates.lisp`:
```lisp
(defparameter *tools-template*
  '[{
     "name": "your_tool",
     "description": "Your tool description",
     "inputSchema": {
       "type": "object",
       "properties": {
         "param": {
           "type": "string",
           "description": "Parameter description"
         }
       },
       "required": ["param"]
     }
   }])
```

2. Implement handler in `tools.lisp`:
```lisp
(defun handle-your-tool (params)
  "Handle your custom tool"
  (let ((param (gethash "param" params)))
    (make-instance 'call-tool-result
                   :content (list (make-instance 'call-tool-result-content
                                                 :type-name "text"
                                                 :text (format nil "Result: ~A" param)))
                   :is-error nil)))
```

3. Add to dispatcher in `main.lisp`:
```lisp
((string= method "your_tool")
 (handle-your-tool params))
```

### Adding New Prompts

Follow similar patterns in `prompts.lisp` and update the templates.

### Adding New Resources

Follow similar patterns in `resources.lisp` and update the templates.

## Architecture

### Key Components

1. **Type System** (`types.lisp`) - Complete MCP protocol type definitions with JSON serialization
2. **Request Dispatcher** (`main.lisp`) - JSON-RPC request routing and handling
3. **Tools Framework** (`tools.lisp`) - Extensible tool registration and execution
4. **Templates** (`templates.lisp`) - JSON-based configuration system
5. **Utilities** (`utilities.lisp`) - Core MCP protocol handlers

### JSON-RPC Handling

The server implements a complete JSON-RPC 2.0 system with:
- Request/response handling
- Notification support
- Error handling with standard error codes
- Batch request support (planned)

### Signal Handling

Graceful shutdown is supported via:
- SIGINT (Ctrl+C) handling
- SIGTERM handling
- Cleanup routines

## Testing

The server includes comprehensive tests covering:
- Type system and JSON serialization
- Tool execution
- Prompt generation
- Resource access
- Protocol compliance

## Deployment

### System Installation

```bash
# Build optimized binary
make release

# Install system-wide
sudo cp mcp-server /usr/local/bin/

# Or use installation script
./install.sh
```

### Distribution

The server can be distributed as:
1. **Standalone Binary** - Single executable with all dependencies
2. **Source Distribution** - For development and customization
3. **Docker Container** - Containerized deployment (planned)

## Performance Considerations

- **Memory Usage** - Efficient JSON parsing with minimal allocations
- **Response Time** - Fast request dispatch with compiled handlers
- **Concurrency** - Thread-safe design for multiple concurrent requests
- **Resource Management** - Proper cleanup and garbage collection

## Debugging

### Logging

- Server logs to `/tmp/mcp.jsonl`
- Use `*debug-mode*` for verbose output
- JSON-RPC messages are logged for inspection

### Common Issues

1. **Missing Dependencies** - Ensure all required libraries are installed via Quicklisp
2. **Signal Handling** - Some Lisp implementations may require different signal handling
3. **JSON Encoding** - Check for proper UTF-8 encoding in responses

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

This project is licensed under the Apache License 2.0. See the LICENSE file for details.

## Differences from Rust Version

### Architecture Changes

1. **Object System** - Uses CLOS (Common Lisp Object System) instead of Rust structs
2. **Error Handling** - Uses Common Lisp condition system instead of Result types
3. **Memory Management** - Garbage collected instead of ownership system
4. **Concurrency** - Uses threads instead of async/await (can be extended with async libraries)

### Feature Parity

- ✅ Complete MCP protocol implementation
- ✅ JSON-RPC 2.0 support
- ✅ Tools, prompts, and resources
- ✅ Signal handling and graceful shutdown
- ✅ Command line interface
- ✅ Claude Desktop integration
- ✅ Logging and debugging support

### Extensions

- **Interactive Development** - REPL-driven development and debugging
- **Hot Reloading** - Dynamic redefinition of handlers during development
- **Lisp Macros** - Powerful metaprogramming for extending functionality
- **Package System** - Clean namespace management

## Roadmap

- [ ] Async I/O support (using iolib or similar)
- [ ] WebSocket transport in addition to stdio
- [ ] Plugin system for dynamic tool loading
- [ ] Docker containerization
- [ ] Performance benchmarking
- [ ] Additional Lisp implementation support (CCL, ECL, etc.)

---

*Built with ❤️ and 🐍 (parentheses) for the MCP ecosystem*
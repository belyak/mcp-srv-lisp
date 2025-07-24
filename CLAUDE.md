# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Common Lisp implementation of an MCP (Model Context Protocol) server, ported from the original Rust version. MCP is a protocol for communication between AI models and external tools/resources.

## Common Development Commands

### Building
```bash
# Build the MCP server binary
make build

# Build optimized release version
make release

# Install dependencies only
make deps
```

### Testing
```bash
# Run quick tests (Lisp + known working e2e tests)
make test

# Run full test suite (all Lisp + all e2e tests)
make test-all

# Run only Lisp unit tests
make test-lisp

# Run specific e2e test categories
make test-e2e-protocol
make test-e2e-tools
make test-e2e-prompts
make test-e2e-resources
```

### Development
```bash
# Start development REPL with project loaded
make dev

# Run the server in MCP mode
./mcp-server --mcp

# List available tools/prompts/resources
./mcp-server --tools --prompts --resources
```

### Linting and Type Checking
The project uses SBCL's built-in type checking and warnings. No separate lint command is needed - compilation will report issues.

## Architecture

### Core Structure
The project follows a modular design with clear separation of concerns:

1. **MCP Protocol Layer** (`src/types.lisp`, `src/utilities.lisp`)
   - Complete JSON-RPC 2.0 implementation
   - Type definitions for all MCP protocol messages
   - Request/response handling infrastructure

2. **Feature Modules** (`src/tools.lisp`, `src/prompts.lisp`, `src/resources.lisp`)
   - Each feature (tools, prompts, resources) has its own module
   - Handlers follow a consistent pattern
   - Easy to extend with new functionality

3. **Main Server** (`src/main.lisp`)
   - Request dispatcher
   - CLI argument parsing
   - Server lifecycle management
   - Signal handling for graceful shutdown

4. **Configuration** (`src/templates.lisp`)
   - JSON-based templates for tools, prompts, and resources
   - Centralized configuration for easy modification

### Key Design Patterns

1. **CLOS-based Type System**: All MCP protocol types are defined as CLOS classes with automatic JSON serialization/deserialization.

2. **Handler Pattern**: Each MCP method has a corresponding handler function that processes requests and returns typed responses.

3. **Template-driven Features**: Tools, prompts, and resources are defined in JSON templates, making it easy to add new ones without modifying core logic.

## Adding New Features

### Adding a New Tool
1. Update `*tools-template*` in `src/templates.lisp` with the tool definition
2. Implement the handler function in `src/tools.lisp`
3. Add a case in the dispatcher in `src/main.lisp`

### Adding a New Prompt
1. Update `*prompts-template*` in `src/templates.lisp`
2. Implement the handler in `src/prompts.lisp` if needed

### Adding a New Resource
1. Update `*resources-template*` in `src/templates.lisp`
2. Implement the handler in `src/resources.lisp` if needed

## Important Notes

- The server logs to `/tmp/mcp.jsonl` for debugging
- Use absolute paths when working with file operations
- The project supports SBCL primarily, with potential support for CCL and ECL
- JSON encoding/decoding is handled automatically by the type system
- Signal handling (SIGINT/SIGTERM) is implemented for graceful shutdown
# Testing Guide

This document describes the comprehensive testing system for the MCP Common Lisp Server.

## Overview

The project includes both Lisp unit tests and Python end-to-end (e2e) tests, all managed through the Makefile build system.

## Test Categories

### 1. Lisp Unit Tests
- Located in `tests/` directory
- Test individual Common Lisp components
- Run using ASDF test system

### 2. Python End-to-End Tests
- Located in `tests/e2e/` directory
- Test the complete MCP protocol implementation
- Use pytest for test execution
- Test real server communication via JSON-RPC

## Quick Start

```bash
# Run full test suite (recommended - comprehensive coverage)
make test-all

# Run quick subset (Lisp + known working e2e tests)
make test

# Run only the working e2e tests (fastest integration test)
make test-e2e-quick
```

## Available Test Targets

### Main Test Targets
- `make test-all` - **RECOMMENDED**: Run comprehensive test suite (Lisp + full e2e)
- `make test` - Run quick subset (Lisp + known working e2e tests)
- `make test-verbose` - Run comprehensive tests with verbose output

### Lisp Test Targets
- `make test-lisp` - Run only Lisp unit tests
- `make test-lisp-verbose` - Run Lisp tests with verbose output

### Python E2E Test Targets
- `make test-e2e` - Run all Python e2e tests
- `make test-e2e-quick` - Run known working e2e tests (fastest)
- `make test-e2e-protocol` - Run protocol e2e tests
- `make test-e2e-tools` - Run tools e2e tests
- `make test-e2e-prompts` - Run prompts e2e tests
- `make test-e2e-resources` - Run resources e2e tests

### Environment Management
- `make venv` - Create Python virtual environment
- `make install-python-deps` - Install Python test dependencies
- `make clean` - Clean all build artifacts and virtual environment

## Test Dependencies

### Automatic Setup
All test targets automatically handle:
- Lisp dependency installation via Quicklisp
- Python virtual environment creation
- Python test dependency installation
- MCP server binary building

### Manual Setup
If you want to set up the environment manually:

```bash
# Set up Python environment
make venv
make install-python-deps

# Build the server
make build
```

## Working Tests

The following e2e tests are currently working and included in `test-e2e-quick`:

✅ **Protocol Tests:**
- `test_server_startup` - Server starts successfully
- `test_initialization_handshake` - MCP initialization works
- `test_ping_functionality` - Ping/pong communication works

✅ **Tools Tests:**
- `test_list_tools` - Tool listing returns proper JSON objects

## Test Output

### Successful Test Run
```
============================================ test session starts ============================================
platform darwin -- Python 3.13.5, pytest-8.4.1, pluggy-1.6.0
collecting ... collected 4 items

test_protocol.py::TestMCPProtocol::test_server_startup PASSED                     [ 25%]
test_protocol.py::TestMCPProtocol::test_initialization_handshake PASSED           [ 50%]
test_protocol.py::TestMCPProtocol::test_ping_functionality PASSED                 [ 75%]
test_tools.py::TestMCPTools::test_list_tools PASSED                              [100%]

============================================= 4 passed in 0.64s =============================================
```

## Environment Variables

- `LISP` - Lisp implementation to use (default: sbcl)
- `PYTHON` - Python executable to use (default: python3)

## Troubleshooting

### Clean Build Issues
If you encounter build or test issues:

```bash
make clean
make test
```

### Python Virtual Environment Issues
If Python dependencies seem out of date:

```bash
make clean
make install-python-deps
```

### Lisp Dependency Issues
If Lisp dependencies are not loading:

```bash
make deps
```

## Adding New Tests

### Adding Lisp Tests
1. Create test files in `tests/` directory
2. Follow ASDF test system conventions
3. Tests will be automatically included in `make test-lisp`

### Adding Python E2E Tests
1. Create test files in `tests/e2e/` directory
2. Follow pytest conventions with `test_*.py` naming
3. Tests will be automatically included in `make test-e2e`

## Continuous Integration

For CI environments, use:

```bash
# For comprehensive CI testing (recommended)
make test-all

# For quick CI feedback (if time constraints)
make test
```

The test system is designed to work from a clean checkout with no manual setup required.

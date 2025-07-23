# =================================================================
# Makefile for MCP Common Lisp Server
# =================================================================

LISP ?= sbcl
BINARY_NAME = mcp-server
BUILD_DIR = build
SRC_DIR = src
TEST_DIR = tests

# Default target
.PHONY: all
all: build

# =================================================================
# Build Targets
# =================================================================

.PHONY: build
build: deps
	@echo "Building MCP server..."
	$(LISP) --load build.lisp --quit
	@echo "Build complete: $(BINARY_NAME)"

.PHONY: release
release: deps
	@echo "Building optimized release..."
	$(LISP) --eval "(setf *compile-print* nil *compile-verbose* nil)" \
		--load build.lisp --quit
	@echo "Release build complete: $(BINARY_NAME)"

.PHONY: deps
deps:
	@echo "Installing dependencies..."
	$(LISP) --eval "(ql:quickload :mcp-server)" --quit

# =================================================================
# Development Targets
# =================================================================

.PHONY: dev
dev:
	@echo "Starting development REPL..."
	$(LISP) --eval "(ql:quickload :mcp-server)" \
		--eval "(in-package :mcp-server)"

.PHONY: repl
repl: dev

# =================================================================
# Testing Targets
# =================================================================

.PHONY: test
test:
	@echo "Running tests..."
	$(LISP) --eval "(ql:quickload :mcp-server)" \
		--eval "(asdf:test-system :mcp-server)" \
		--quit

.PHONY: test-verbose
test-verbose:
	@echo "Running tests with verbose output..."
	$(LISP) --eval "(ql:quickload :mcp-server)" \
		--eval "(setf *print-failures* t *print-errors* t)" \
		--eval "(asdf:test-system :mcp-server)" \
		--quit

# =================================================================
# Utility Targets
# =================================================================

.PHONY: clean
clean:
	@echo "Cleaning build artifacts..."
	rm -f $(BINARY_NAME)
	rm -rf $(BUILD_DIR)
	find . -name "*.fasl" -delete
	find . -name "*.fas" -delete
	find . -name "*.lib" -delete
	find . -name "*~" -delete

.PHONY: install
install: build
	@echo "Installing MCP server..."
	sudo cp $(BINARY_NAME) /usr/local/bin/
	sudo chmod +x /usr/local/bin/$(BINARY_NAME)
	@echo "Installation complete: /usr/local/bin/$(BINARY_NAME)"

.PHONY: uninstall
uninstall:
	@echo "Uninstalling MCP server..."
	sudo rm -f /usr/local/bin/$(BINARY_NAME)
	@echo "Uninstallation complete"

# =================================================================
# Testing Individual Components
# =================================================================

.PHONY: test-tools
test-tools:
	$(LISP) --eval "(ql:quickload :mcp-server)" \
		--eval "(load \"$(TEST_DIR)/test-tools.lisp\")" \
		--quit

.PHONY: test-prompts
test-prompts:
	$(LISP) --eval "(ql:quickload :mcp-server)" \
		--eval "(load \"$(TEST_DIR)/test-prompts.lisp\")" \
		--quit

.PHONY: test-resources
test-resources:
	$(LISP) --eval "(ql:quickload :mcp-server)" \
		--eval "(load \"$(TEST_DIR)/test-resources.lisp\")" \
		--quit

# =================================================================
# Documentation and Help
# =================================================================

.PHONY: help
help:
	@echo "MCP Common Lisp Server Build System"
	@echo ""
	@echo "Available targets:"
	@echo "  build        - Build the MCP server binary"
	@echo "  release      - Build optimized release binary"
	@echo "  deps         - Install dependencies"
	@echo "  dev/repl     - Start development REPL"
	@echo "  test         - Run all tests"
	@echo "  test-verbose - Run tests with verbose output"
	@echo "  clean        - Clean build artifacts"
	@echo "  install      - Install binary to /usr/local/bin"
	@echo "  uninstall    - Remove installed binary"
	@echo "  help         - Show this help message"
	@echo ""
	@echo "Environment variables:"
	@echo "  LISP         - Lisp implementation to use (default: sbcl)"

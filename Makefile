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
	$(LISP) --eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:register-local-projects)" \
		--eval "(ql:quickload :mcp-server)" --quit

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

# Python testing variables
PYTHON ?= python3
VENV_DIR = .venv
VENV_PYTHON = $(VENV_DIR)/bin/python
VENV_PIP = $(VENV_DIR)/bin/pip
E2E_DIR = tests/e2e

# Create Python virtual environment
.PHONY: venv
venv:
	@echo "Creating Python virtual environment..."
	$(PYTHON) -m venv $(VENV_DIR)
	@echo "Virtual environment created at $(VENV_DIR)"

# Install Python dependencies
.PHONY: install-python-deps
install-python-deps: venv
	@echo "Installing Python test dependencies..."
	$(VENV_PIP) install --upgrade pip
	$(VENV_PIP) install -r $(E2E_DIR)/requirements.txt
	@echo "Python dependencies installed"

# Run Lisp unit tests
.PHONY: test-lisp
test-lisp: deps
	@echo "Running Lisp unit tests..."
	$(LISP) --eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:register-local-projects)" \
		--eval "(ql:quickload :mcp-server)" \
		--eval "(asdf:test-system :mcp-server)" \
		--quit

# Run Lisp unit tests with GitHub Actions formatting
.PHONY: test-lisp-github
test-lisp-github: deps
	@echo "Running Lisp unit tests with GitHub formatting..."
	@echo "::group::Lisp Unit Tests"
	$(LISP) --eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:register-local-projects)" \
		--eval "(ql:quickload :mcp-server)" \
		--eval "(ql:quickload :mcp-server/tests)" \
		--eval "(defparameter *github-mode* t)" \
		--eval "(in-package :mcp-server-tests)" \
		--eval "(defun github-format-output (output) \
		          (let ((lines (split-sequence:split-sequence #\Newline output))) \
		            (dolist (line lines) \
		              (cond \
		                ((search \"✓\" line) \
		                 (format t \"::notice::~A~%\" line)) \
		                ((search \"✗\" line) \
		                 (format t \"::error::~A~%\" line)) \
		                (t (format t \"~A~%\" line))))))" \
		--eval "(let ((output (make-string-output-stream))) \
		          (let ((*standard-output* output)) \
		            (run-all-tests)) \
		          (github-format-output (get-output-stream-string output)))" \
		--quit || (echo "::error::Lisp unit tests failed" && exit 1)
	@echo "::endgroup::"

# Run Lisp tests with verbose output
.PHONY: test-lisp-verbose
test-lisp-verbose: deps
	@echo "Running Lisp tests with verbose output..."
	$(LISP) --eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:register-local-projects)" \
		--eval "(ql:quickload :mcp-server)" \
		--eval "(setf *print-failures* t *print-errors* t)" \
		--eval "(asdf:test-system :mcp-server)" \
		--quit

# Run Python e2e tests
.PHONY: test-e2e
test-e2e: build install-python-deps
	@echo "Running Python e2e tests..."
	cd $(E2E_DIR) && ../../$(VENV_PYTHON) -m pytest . -v

# Run Python e2e tests with GitHub Actions formatting
.PHONY: test-e2e-github
test-e2e-github: build install-python-deps
	@echo "Running Python e2e tests with GitHub formatting..."
	@echo "::group::End-to-End Tests"
	cd $(E2E_DIR) && ../../$(VENV_PYTHON) -m pytest . -v \
		--tb=short \
		--junit-xml=../../test-results-e2e.xml \
		-o junit_logging=all \
		-o log_cli=true \
		-o log_cli_level=INFO \
		--capture=no \
		|| (echo "::error::E2E tests failed" && exit 1)
	@echo "::endgroup::"

# Run fastmcp reference server
.PHONY: run-fastmcp-ref
run-fastmcp-ref: install-python-deps
	@echo "Running fastmcp reference server..."
	$(VENV_DIR)/bin/fastmcp run ./tests/e2e/py-references/fastmcp_memdb.py

# Run specific Python e2e test files
.PHONY: test-e2e-protocol
test-e2e-protocol: build install-python-deps
	@echo "Running protocol e2e tests..."
	cd $(E2E_DIR) && ../../$(VENV_PYTHON) -m pytest test_protocol.py -v

.PHONY: test-e2e-tools
test-e2e-tools: build install-python-deps
	@echo "Running tools e2e tests..."
	cd $(E2E_DIR) && ../../$(VENV_PYTHON) -m pytest test_tools.py -v

.PHONY: test-e2e-prompts
test-e2e-prompts: build install-python-deps
	@echo "Running prompts e2e tests..."
	cd $(E2E_DIR) && ../../$(VENV_PYTHON) -m pytest test_prompts.py -v

.PHONY: test-e2e-resources
test-e2e-resources: build install-python-deps
	@echo "Running resources e2e tests..."
	cd $(E2E_DIR) && ../../$(VENV_PYTHON) -m pytest test_resources.py -v

# Run quick e2e tests (known working tests)
.PHONY: test-e2e-quick
test-e2e-quick: build install-python-deps
	@echo "Running quick e2e tests (known working tests)..."
	cd $(E2E_DIR) && ../../$(VENV_PYTHON) -m pytest \
		test_protocol.py::TestMCPProtocol::test_server_startup \
		test_protocol.py::TestMCPProtocol::test_initialization_handshake \
		test_protocol.py::TestMCPProtocol::test_ping_functionality \
		test_tools.py::TestMCPTools::test_list_tools \
		-v

# Run quick e2e tests with GitHub Actions formatting
.PHONY: test-e2e-quick-github
test-e2e-quick-github: build install-python-deps
	@echo "Running quick e2e tests with GitHub formatting..."
	@echo "::group::Quick End-to-End Tests"
	cd $(E2E_DIR) && ../../$(VENV_PYTHON) -m pytest \
		test_protocol.py::TestMCPProtocol::test_server_startup \
		test_protocol.py::TestMCPProtocol::test_initialization_handshake \
		test_protocol.py::TestMCPProtocol::test_ping_functionality \
		test_tools.py::TestMCPTools::test_list_tools \
		-v --tb=short \
		--junit-xml=../../test-results-e2e-quick.xml \
		-o junit_logging=all \
		-o log_cli=true \
		-o log_cli_level=INFO \
		--capture=no \
		|| (echo "::error::Quick E2E tests failed" && exit 1)
	@echo "::endgroup::"

# Run comprehensive test suite (recommended default)
.PHONY: test
test: test-lisp test-e2e
	@echo "Comprehensive test suite completed"

# Run comprehensive test suite with GitHub Actions formatting
.PHONY: test-github
test-github: test-lisp-github test-e2e-github
	@echo "::notice::Comprehensive test suite completed successfully"

# Run quick test subset (Lisp + known working e2e tests)
.PHONY: test-quick
test-quick: test-lisp test-e2e-quick
	@echo "Quick test subset completed"

# Run quick test subset with GitHub Actions formatting
.PHONY: test-quick-github
test-quick-github: test-lisp-github test-e2e-quick-github
	@echo "::notice::Quick test subset completed successfully"

# Alias for comprehensive test suite (for backward compatibility)
.PHONY: test-all
test-all: test
	@echo "Full test suite completed"

.PHONY: test-verbose
test-verbose: test-lisp-verbose test-e2e
	@echo "Verbose test suite completed"

# =================================================================
# Utility Targets
# =================================================================

.PHONY: clean
clean:
	@echo "Cleaning build artifacts..."
	rm -f $(BINARY_NAME)
	rm -rf $(BUILD_DIR)
	rm -rf $(VENV_DIR)
	find . -name "*.fasl" -delete
	find . -name "*.fas" -delete
	find . -name "*.lib" -delete
	find . -name "*~" -delete
	find . -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
	find . -name "*.pyc" -delete
	find . -name ".pytest_cache" -type d -exec rm -rf {} + 2>/dev/null || true

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
	@echo "Build targets:"
	@echo "  build         - Build the MCP server binary"
	@echo "  release       - Build optimized release binary"
	@echo "  deps          - Install Lisp dependencies"
	@echo "  clean         - Clean build artifacts and virtual environment"
	@echo ""
	@echo "Development targets:"
	@echo "  dev/repl      - Start development REPL"
	@echo ""
	@echo "Testing targets:"
	@echo "  test          - Run comprehensive test suite (recommended)"
	@echo "  test-quick    - Run quick subset (Lisp + known working e2e tests)"
	@echo "  test-all      - Alias for comprehensive test suite"
	@echo "  test-verbose  - Run comprehensive tests with verbose output"
	@echo "  test-lisp     - Run only Lisp unit tests"
	@echo "  test-e2e      - Run all Python e2e tests"
	@echo "  test-e2e-quick- Run known working e2e tests"
	@echo ""
	@echo "GitHub CI/CD testing targets:"
	@echo "  test-github        - Run comprehensive tests with GitHub formatting"
	@echo "  test-quick-github  - Run quick tests with GitHub formatting"
	@echo "  test-lisp-github   - Run Lisp tests with GitHub formatting"
	@echo "  test-e2e-github    - Run e2e tests with GitHub formatting"
	@echo "  test-e2e-quick-github - Run quick e2e tests with GitHub formatting"
	@echo ""
	@echo "Specific e2e test targets:"
	@echo "  test-e2e-protocol  - Run protocol e2e tests"
	@echo "  test-e2e-tools     - Run tools e2e tests"
	@echo "  test-e2e-prompts   - Run prompts e2e tests"
	@echo "  test-e2e-resources - Run resources e2e tests"
	@echo ""
	@echo "Python environment targets:"
	@echo "  venv               - Create Python virtual environment"
	@echo "  install-python-deps- Install Python test dependencies"
	@echo "  run-fastmcp-ref    - Run fastmcp reference server"
	@echo ""
	@echo "Installation targets:"
	@echo "  install       - Install binary to /usr/local/bin"
	@echo "  uninstall     - Remove installed binary"
	@echo ""
	@echo "Environment variables:"
	@echo "  LISP          - Lisp implementation to use (default: sbcl)"
	@echo "  PYTHON        - Python executable to use (default: python3)"

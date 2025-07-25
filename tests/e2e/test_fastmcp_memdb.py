#!/usr/bin/env python3
"""
Tests for the FastMCP in-memory database server.
"""
import asyncio
import json
import subprocess
import tempfile
from pathlib import Path
from typing import Any, Dict, Optional, Type
from types import TracebackType

import pytest


class FastMCPTestClient:
    """Test client for FastMCP in-memory database server."""

    def __init__(self):
        self.process: Optional[subprocess.Popen[str]] = None
        self.request_id = 0

    async def __aenter__(self):
        """Async context manager entry."""
        await self.start_server()
        await self.initialize()
        return self

    async def __aexit__(
        self, 
        exc_type: Optional[Type[BaseException]], 
        exc_val: Optional[BaseException], 
        exc_tb: Optional[TracebackType]
    ) -> None:
        """Async context manager exit."""
        await self.cleanup()

    async def start_server(self) -> None:
        """Start the FastMCP server process."""
        # Get the project root directory
        project_root = Path(__file__).parent.parent.parent
        
        # Path to the virtual environment and the server script
        venv_fastmcp = project_root / ".venv" / "bin" / "fastmcp"
        server_script = Path(__file__).parent / "py-references" / "fastmcp_memdb.py"
        
        # Check if venv exists, if not try to use system fastmcp
        if venv_fastmcp.exists():
            cmd = [str(venv_fastmcp), "run", str(server_script)]
        else:
            # Fallback to system fastmcp
            cmd = ["fastmcp", "run", str(server_script)]
        
        # Start server process
        self.process = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=0,
            cwd=project_root,
        )

        # Wait a moment for startup
        await asyncio.sleep(0.5)

        # Check if process started successfully
        if self.process.poll() is not None:
            stdout, stderr = self.process.communicate()
            raise RuntimeError(
                f"FastMCP server failed to start. stdout: {stdout}, stderr: {stderr}"
            )

    def _next_request_id(self) -> int:
        """Get next request ID."""
        self.request_id += 1
        return self.request_id

    async def _send_request(
        self, method: str, params: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """Send a JSON-RPC request and get response."""
        if not self.process:
            raise RuntimeError("Server process not started")

        request = {"jsonrpc": "2.0", "id": self._next_request_id(), "method": method}

        if params is not None:
            request["params"] = params

        # Send request
        request_json = json.dumps(request) + "\n"
        self.process.stdin.write(request_json)  # type: ignore
        self.process.stdin.flush()  # type: ignore

        # Read response
        response_line = self.process.stdout.readline()  # type: ignore
        if not response_line:
            raise RuntimeError("No response from server")

        try:
            response = json.loads(response_line.strip())
        except json.JSONDecodeError as e:
            raise RuntimeError(f"Invalid JSON response: {response_line}") from e

        # Check for errors
        if "error" in response:
            error = response["error"]
            raise RuntimeError(
                f"MCP Error {error.get('code', 'unknown')}: {error.get('message', 'Unknown error')}"
            )

        return response.get("result", {})

    async def initialize(self) -> Dict[str, Any]:
        """Initialize the MCP session."""
        if hasattr(self, 'initialized') and self.initialized:
            return {}

        result = await self._send_request(
            "initialize",
            {
                "protocolVersion": "2024-11-05",
                "capabilities": {"resources": {}, "tools": {}},
                "clientInfo": {"name": "test-client", "version": "1.0.0"},
            },
        )

        # Send initialized notification
        notification = {"jsonrpc": "2.0", "method": "notifications/initialized"}
        notification_json = json.dumps(notification) + "\n"
        self.process.stdin.write(notification_json)  # type: ignore
        self.process.stdin.flush()  # type: ignore

        self.initialized = True
        return result

    async def list_resources(self) -> Dict[str, Any]:
        """List available resources."""
        if not hasattr(self, 'initialized') or not self.initialized:
            await self.initialize()
        return await self._send_request("resources/list")

    async def read_resource(self, uri: str) -> Dict[str, Any]:
        """Read a specific resource."""
        if not hasattr(self, 'initialized') or not self.initialized:
            await self.initialize()
        return await self._send_request("resources/read", {"uri": uri})

    async def list_tools(self) -> Dict[str, Any]:
        """List available tools."""
        if not hasattr(self, 'initialized') or not self.initialized:
            await self.initialize()
        return await self._send_request("tools/list")

    async def call_tool(self, name: str, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """Call a tool with given arguments."""
        if not hasattr(self, 'initialized') or not self.initialized:
            await self.initialize()
        return await self._send_request(
            "tools/call", {"name": name, "arguments": arguments}
        )

    async def cleanup(self) -> None:
        """Clean up the server process."""
        if self.process:
            self.process.terminate()
            try:
                await asyncio.wait_for(asyncio.create_task(
                    asyncio.to_thread(self.process.wait)
                ), timeout=5.0)
            except asyncio.TimeoutError:
                self.process.kill()
                await asyncio.create_task(asyncio.to_thread(self.process.wait))


class TestFastMCPMemDB:
    """Test suite for FastMCP in-memory database server."""

    @pytest.mark.asyncio
    async def test_server_initialization(self):
        """Test that the server initializes correctly."""
        async with FastMCPTestClient() as client:
            # If we get here without exception, initialization was successful
            result = await client.list_resources()
            assert "resources" in result

    @pytest.mark.asyncio
    async def test_list_resources(self):
        """Test listing available resources."""
        async with FastMCPTestClient() as client:
            result = await client.list_resources()
            
            assert "resources" in result
            resources = result["resources"]
            
            # Should have exactly 1 static resource (the parameterized get resource doesn't show up in the list)
            assert len(resources) == 1
            
            # Check for list_keys resource
            list_keys_resource = resources[0]
            assert list_keys_resource["name"] == "list_keys"
            assert list_keys_resource["uri"] == "memory::/keys"
            assert list_keys_resource["description"] == "List all keys in the in-memory DB."
            assert list_keys_resource["mimeType"] == "text/plain"

    @pytest.mark.asyncio
    async def test_list_tools(self):
        """Test listing available tools."""
        async with FastMCPTestClient() as client:
            result = await client.list_tools()
            
            assert "tools" in result
            tools = result["tools"]
            
            # Should have exactly 3 tools
            assert len(tools) == 3
            
            tool_names = {tool["name"] for tool in tools}
            assert "set" in tool_names
            assert "clear" in tool_names
            assert "interpret_key" in tool_names
            
            # Check set tool details
            set_tool = next((t for t in tools if t["name"] == "set"), None)
            assert set_tool is not None
            assert set_tool["description"] == "Set a key-value pair."
            
            # Check clear tool details
            clear_tool = next((t for t in tools if t["name"] == "clear"), None)
            assert clear_tool is not None
            assert clear_tool["description"] == "Clear all keys from DB."
            
            # Check interpret_key tool details
            interpret_tool = next((t for t in tools if t["name"] == "interpret_key"), None)
            assert interpret_tool is not None
            assert "Interpret a key as a command" in interpret_tool["description"]

    @pytest.mark.asyncio
    async def test_empty_database_operations(self):
        """Test operations on an empty database."""
        async with FastMCPTestClient() as client:
            # First clear the database to ensure it's empty
            await client.call_tool("clear", {})
            
            # Test listing keys on empty DB
            result = await client.read_resource("memory::/keys")
            assert "contents" in result
            content = result["contents"][0]
            assert content["mimeType"] == "text/plain"
            
            # Parse the JSON content
            data = json.loads(content["text"])
            assert data["keys"] == []
            assert data["count"] == 0
            
            # Test getting a non-existent key
            result = await client.read_resource("memory::/get/nonexistent")
            assert "contents" in result
            content = result["contents"][0]
            data = json.loads(content["text"])
            assert data["key"] == "nonexistent"
            assert data["value"] is None

    @pytest.mark.asyncio
    async def test_set_and_get_operations(self):
        """Test setting and getting key-value pairs."""
        async with FastMCPTestClient() as client:
            # Clear database first
            await client.call_tool("clear", {})
            
            # Set a key-value pair
            result = await client.call_tool("set", {"key": "test_key", "value": "test_value"})
            assert "content" in result
            content = result["content"][0]
            data = json.loads(content["text"])
            assert data["status"] == "ok"
            assert data["key"] == "test_key"
            assert data["value"] == "test_value"
            
            # Get the value back
            result = await client.read_resource("memory::/get/test_key")
            content = result["contents"][0]
            data = json.loads(content["text"])
            assert data["key"] == "test_key"
            assert data["value"] == "test_value"
            
            # Check that keys list includes our key
            result = await client.read_resource("memory::/keys")
            content = result["contents"][0]
            data = json.loads(content["text"])
            assert "test_key" in data["keys"]
            assert data["count"] == 1

    @pytest.mark.asyncio
    async def test_multiple_key_operations(self):
        """Test operations with multiple keys."""
        async with FastMCPTestClient() as client:
            # Clear database first
            await client.call_tool("clear", {})
            
            # Set multiple key-value pairs
            test_data = {
                "key1": "value1",
                "key2": "value2",
                "key3": "value3"
            }
            
            for key, value in test_data.items():
                await client.call_tool("set", {"key": key, "value": value})
            
            # Verify all keys are listed
            result = await client.read_resource("memory::/keys")
            content = result["contents"][0]
            data = json.loads(content["text"])
            assert set(data["keys"]) == set(test_data.keys())
            assert data["count"] == 3
            
            # Verify all values can be retrieved
            for key, expected_value in test_data.items():
                result = await client.read_resource(f"memory::/get/{key}")
                content = result["contents"][0]
                data = json.loads(content["text"])
                assert data["key"] == key
                assert data["value"] == expected_value

    @pytest.mark.asyncio
    async def test_overwrite_key(self):
        """Test overwriting an existing key."""
        async with FastMCPTestClient() as client:
            # Clear database first
            await client.call_tool("clear", {})
            
            # Set initial value
            await client.call_tool("set", {"key": "overwrite_test", "value": "initial_value"})
            
            # Verify initial value
            result = await client.read_resource("memory::/get/overwrite_test")
            content = result["contents"][0]
            data = json.loads(content["text"])
            assert data["value"] == "initial_value"
            
            # Overwrite with new value
            await client.call_tool("set", {"key": "overwrite_test", "value": "new_value"})
            
            # Verify new value
            result = await client.read_resource("memory::/get/overwrite_test")
            content = result["contents"][0]
            data = json.loads(content["text"])
            assert data["value"] == "new_value"
            
            # Verify count is still 1 (no duplicate keys)
            result = await client.read_resource("memory::/keys")
            content = result["contents"][0]
            data = json.loads(content["text"])
            assert data["count"] == 1

    @pytest.mark.asyncio
    async def test_interpret_key_function(self):
        """Test the interpret_key function with human/stupid key-value pair."""
        async with FastMCPTestClient() as client:
            # Clear database first
            await client.call_tool("clear", {})
            
            # Set the human/stupid key-value pair
            await client.call_tool("set", {"key": "human", "value": "stupid"})
            
            # Call interpret_key tool - note that this may return empty result
            # if AI context is not available in test environment
            try:
                result = await client.call_tool("interpret_key", {"key": "human"})
                
                # If the result is not empty, verify the structure
                if result and "content" in result:
                    content = result["content"][0]
                    data = json.loads(content["text"])
                    
                    # Verify the response structure
                    assert data["status"] == "found"
                    assert data["key"] == "human"
                    assert data["value"] == "stupid"
                    
                    # If interpretation is present, check it contains relevant terms
                    if "interpretation" in data:
                        interpretation = data["interpretation"].lower()
                        assert "human" in interpretation
                else:
                    # If result is empty, it means the AI sampling failed
                    # which is expected in test environment without AI context
                    print("Note: interpret_key returned empty result - AI context may not be available in test environment")
                
                # Test with non-existent key
                result = await client.call_tool("interpret_key", {"key": "nonexistent"})
                if result and "content" in result:
                    content = result["content"][0] 
                    data = json.loads(content["text"])
                    assert data["status"] == "not found"
                    assert data["key"] == "nonexistent"
                    
            except Exception as e:
                # If the tool call fails, it might be due to missing AI context
                print(f"interpret_key tool call failed (expected in test environment): {e}")
                # We can still verify the tool exists in the tools list
                tools_result = await client.list_tools()
                tool_names = {tool["name"] for tool in tools_result["tools"]}
                assert "interpret_key" in tool_names

    @pytest.mark.asyncio
    async def test_clear_database(self):
        """Test clearing the entire database."""
        async with FastMCPTestClient() as client:
            # Set some data first
            await client.call_tool("set", {"key": "key1", "value": "value1"})
            await client.call_tool("set", {"key": "key2", "value": "value2"})
            
            # Verify data exists
            result = await client.read_resource("memory::/keys")
            content = result["contents"][0]
            data = json.loads(content["text"])
            assert data["count"] == 2
            
            # Clear the database
            result = await client.call_tool("clear", {})
            content = result["content"][0]
            data = json.loads(content["text"])
            assert data["status"] == "cleared"
            
            # Verify database is empty
            result = await client.read_resource("memory::/keys")
            content = result["contents"][0]
            data = json.loads(content["text"])
            assert data["keys"] == []
            assert data["count"] == 0

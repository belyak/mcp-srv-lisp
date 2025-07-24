import asyncio
import json
import logging
import subprocess
from pathlib import Path
from typing import Any, Dict, Optional

logger = logging.getLogger(__name__)


class MCPTestClient:
    """Enhanced MCP client for testing using direct JSON-RPC communication."""

    def __init__(self, client_name: str = "test-client"):
        self.client_name = client_name
        self.process: Optional[subprocess.Popen[str]] = None
        self.initialized = False
        self.request_id = 0

    async def start_server(self) -> None:
        """Start the MCP server process."""
        project_root = Path(__file__).parent.parent.parent
        binary_path = project_root / "mcp-server"

        # Ensure binary exists
        if not binary_path.exists():
            logger.info("Building MCP server...")
            build_result = subprocess.run(
                ["make", "build"],
                check=False,
                cwd=project_root,
                capture_output=True,
                text=True,
            )
            if build_result.returncode != 0:
                raise RuntimeError(f"Failed to build MCP server: {build_result.stderr}")

        # Start server process
        self.process = subprocess.Popen(
            [str(binary_path), "--mcp"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=0,
        )

        # Wait a moment for startup
        await asyncio.sleep(0.1)

        # Check if process started
        if self.process.poll() is not None:
            stdout, stderr = self.process.communicate()
            raise RuntimeError(
                f"Server failed to start. stdout: {stdout}, stderr: {stderr}"
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
            await self.start_server()

        if not self.process:
            raise RuntimeError("Failed to start server process")

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
        if self.initialized:
            return {}

        result = await self._send_request(
            "initialize",
            {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": self.client_name, "version": "1.0.0"},
            },
        )

        # Send initialized notification
        notification = {"jsonrpc": "2.0", "method": "notifications/initialized"}
        notification_json = json.dumps(notification) + "\n"
        self.process.stdin.write(notification_json)  # type: ignore
        self.process.stdin.flush()  # type: ignore

        self.initialized = True
        return result

    async def list_tools(self) -> Dict[str, Any]:
        """List available tools."""
        if not self.initialized:
            await self.initialize()
        return await self._send_request("tools/list")

    async def call_tool(self, name: str, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """Call a tool with arguments."""
        if not self.initialized:
            await self.initialize()
        return await self._send_request(
            "tools/call", {"name": name, "arguments": arguments}
        )

    async def list_prompts(self) -> Dict[str, Any]:
        """List available prompts."""
        if not self.initialized:
            await self.initialize()
        return await self._send_request("prompts/list")

    async def get_prompt(
        self, name: str, arguments: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """Get a prompt with optional arguments."""
        if not self.initialized:
            await self.initialize()
        params: Dict[str, Any] = {"name": name}
        if arguments:
            params["arguments"] = arguments
        return await self._send_request("prompts/get", params)

    async def list_resources(self) -> Dict[str, Any]:
        """List available resources."""
        if not self.initialized:
            await self.initialize()
        return await self._send_request("resources/list")

    async def read_resource(self, uri: str) -> Dict[str, Any]:
        """Read a resource by URI."""
        if not self.initialized:
            await self.initialize()
        return await self._send_request("resources/read", {"uri": uri})

    async def ping(self) -> Dict[str, Any]:
        """Send ping to server."""
        if not self.initialized:
            await self.initialize()
        return await self._send_request("ping", {})

    async def close(self) -> None:
        """Close connection and cleanup."""
        if self.process and self.process.poll() is None:
            try:
                self.process.terminate()
                self.process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait()

    async def __aenter__(self):
        """Async context manager entry."""
        await self.initialize()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Async context manager exit."""
        await self.close()

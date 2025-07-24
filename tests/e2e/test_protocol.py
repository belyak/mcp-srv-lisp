import asyncio

import pytest
from mcp_client_helper import MCPTestClient


class TestMCPProtocol:
    """Test MCP protocol initialization and basic communication."""

    @pytest.mark.asyncio
    async def test_server_startup(self):
        """Test that the MCP server starts successfully."""
        async with MCPTestClient() as client:
            # If we reach here, the server started successfully
            assert client.initialized

    @pytest.mark.asyncio
    async def test_initialization_handshake(self):
        """Test the MCP initialization handshake."""
        async with MCPTestClient() as client:
            # The initialization should have been completed in the context manager
            assert client.initialized

            # We can't easily access the init result with FastMCP,
            # but we can verify the client is properly initialized
            # by making a simple call that requires initialization
            tools = await client.list_tools()
            assert "tools" in tools

    @pytest.mark.asyncio
    async def test_ping_functionality(self):
        """Test ping/pong functionality."""
        async with MCPTestClient() as client:
            result = await client.ping()
            # Ping should return an empty result successfully
            assert result is not None

    @pytest.mark.asyncio
    async def test_multiple_requests(self):
        """Test that multiple requests work correctly."""
        async with MCPTestClient() as client:
            # Make multiple requests to verify the connection is stable
            for i in range(3):
                result = await client.ping()
                assert result is not None

                tools = await client.list_tools()
                assert "tools" in tools

    @pytest.mark.asyncio
    async def test_server_capabilities(self):
        """Test that server reports correct capabilities."""
        async with MCPTestClient() as client:
            # Test that all expected capabilities are available

            # Tools capability
            tools = await client.list_tools()
            assert "tools" in tools
            assert len(tools["tools"]) > 0

            # Prompts capability
            prompts = await client.list_prompts()
            assert "prompts" in prompts
            assert len(prompts["prompts"]) > 0

            # Resources capability
            resources = await client.list_resources()
            assert "resources" in resources
            assert len(resources["resources"]) > 0

    @pytest.mark.asyncio
    async def test_concurrent_clients(self):
        """Test that multiple clients can connect simultaneously."""

        async def create_client_and_ping():
            current_task = asyncio.current_task()
            task_name = current_task.get_name() if current_task else "unknown"  # type: ignore
            async with MCPTestClient(f"test-client-{task_name}") as client:
                result = await client.ping()
                return result

        # Create multiple concurrent clients
        tasks = [create_client_and_ping() for _ in range(3)]
        results = await asyncio.gather(*tasks)

        # All clients should succeed
        assert len(results) == 3
        for result in results:
            assert result is not None  # Test commit

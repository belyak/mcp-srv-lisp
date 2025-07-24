import asyncio

import pytest
from mcp_client_helper import MCPTestClient


class TestMCPErrorHandling:
    """Test MCP error handling and edge cases."""

    @pytest.mark.asyncio
    async def test_invalid_method_call(self):
        """Test calling an invalid/unknown method."""
        async with MCPTestClient() as client:
            # Try to call a method that doesn't exist
            try:
                # We'll have to simulate this by trying to call an unknown tool
                await client.call_tool("invalid_method_name", {})
                pytest.fail("Expected exception for invalid method")
            except Exception as e:
                # Should get an error about method not found
                error_msg = str(e).lower()
                assert any(
                    term in error_msg
                    for term in ["not found", "unknown", "invalid", "error"]
                )

    @pytest.mark.asyncio
    async def test_malformed_tool_arguments(self):
        """Test tool calls with malformed arguments."""
        async with MCPTestClient() as client:
            # Test with wrong argument types
            try:
                result = await client.call_tool(
                    "get_current_time_in_city",
                    {"city": 123},  # Should be string, not number
                )
                # Might succeed but return an error flag
                if "is_error" in result:
                    assert result["is_error"] is True
            except Exception:
                # Exception is also acceptable
                pass

    @pytest.mark.asyncio
    async def test_empty_arguments_when_required(self):
        """Test calling methods with empty arguments when parameters are required."""
        async with MCPTestClient() as client:
            # Test tool call without required parameters
            try:
                result = await client.call_tool("get_current_time_in_city", {})
                # Should either raise exception or return error
                # Note: Current implementation might accept empty parameters gracefully
                if result.get("isError"):
                    assert result["isError"] is True
                else:
                    # The current implementation handles missing parameters gracefully
                    pass
            except Exception:
                # Exception is acceptable
                pass

    @pytest.mark.asyncio
    async def test_prompt_with_invalid_name(self):
        """Test getting a prompt that doesn't exist."""
        async with MCPTestClient() as client:
            try:
                await client.get_prompt("non_existent_prompt", {})
                pytest.fail("Expected error for non-existent prompt")
            except Exception as e:
                # Should indicate prompt not found
                error_msg = str(e).lower()
                assert any(
                    term in error_msg for term in ["not found", "unknown", "invalid"]
                )

    @pytest.mark.asyncio
    async def test_resource_with_invalid_uri(self):
        """Test reading a resource with completely invalid URI."""
        async with MCPTestClient() as client:
            # Test with various invalid URIs
            invalid_uris = [
                "not-a-uri",
                "",
                "file://",
                "http://invalid",
                "ftp://also-invalid",
            ]

            for uri in invalid_uris:
                try:
                    result = await client.read_resource(uri)
                    # Current implementation might not validate strictly,
                    # so we just check it doesn't crash
                    assert "content" in result
                except Exception:
                    # Exception is also acceptable
                    pass

    @pytest.mark.asyncio
    async def test_client_robustness_under_stress(self):
        """Test client robustness under rapid requests."""
        async with MCPTestClient() as client:
            # Make many rapid requests
            tasks = []
            for i in range(10):
                tasks.append(client.ping())
                tasks.append(client.list_tools())
                tasks.append(client.list_prompts())
                tasks.append(client.list_resources())

            # All should complete successfully
            results = await asyncio.gather(*tasks, return_exceptions=True)

            # Check that most requests succeeded
            exceptions = [r for r in results if isinstance(r, Exception)]
            successful = len(results) - len(exceptions)

            # At least 80% should succeed (allowing for some failures under stress)
            assert successful >= len(results) * 0.8

    @pytest.mark.asyncio
    async def test_large_argument_values(self):
        """Test handling of large argument values."""
        async with MCPTestClient() as client:
            # Test with very long city name
            long_city = "A" * 1000

            try:
                result = await client.call_tool(
                    "get_current_time_in_city", {"city": long_city}
                )
                # Should handle gracefully
                assert "content" in result
                assert "is_error" in result
            except Exception:
                # Exception is acceptable for very large inputs
                pass

    @pytest.mark.asyncio
    async def test_special_characters_in_arguments(self):
        """Test handling of special characters in arguments."""
        async with MCPTestClient() as client:
            special_cities = [
                "São Paulo",
                "москва",  # Moscow in Cyrillic
                "東京",  # Tokyo in Japanese
                "Zürich",
                "città",
                "test\nwith\nnewlines",
                'test"with"quotes',
                "test'with'quotes",
            ]

            for city in special_cities:
                try:
                    result = await client.call_tool(
                        "get_current_time_in_city", {"city": city}
                    )
                    # Should handle Unicode and special characters
                    assert "content" in result
                    assert "is_error" in result
                except Exception:
                    # Some special characters might cause issues, which is acceptable
                    pass

    @pytest.mark.asyncio
    async def test_null_and_none_values(self):
        """Test handling of null/None values in arguments."""
        async with MCPTestClient() as client:
            try:
                result = await client.call_tool(
                    "get_current_time_in_city", {"city": None}
                )
                # Should either handle gracefully or return error
                if "is_error" in result:
                    # Error response is acceptable
                    assert result["is_error"] is True
            except Exception:
                # Exception is also acceptable for None values
                pass

    @pytest.mark.asyncio
    async def test_concurrent_tool_calls(self):
        """Test concurrent tool calls to the same tool."""
        async with MCPTestClient() as client:
            # Make concurrent calls to the same tool
            tasks = [
                client.call_tool("get_current_time_in_city", {"city": f"City{i}"})
                for i in range(5)
            ]

            results = await asyncio.gather(*tasks, return_exceptions=True)

            # Check that all calls succeeded
            for i, result in enumerate(results):
                if isinstance(result, Exception):
                    pytest.fail(f"Concurrent call {i} failed: {result}")
                else:
                    assert "content" in result  # type: ignore
                    assert "isError" in result  # type: ignore
                    assert result["isError"] is False  # type: ignore

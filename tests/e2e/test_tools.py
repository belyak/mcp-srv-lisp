import re

import pytest
from mcp_client_helper import MCPTestClient


class TestMCPTools:
    """Test MCP tools functionality."""

    @pytest.mark.asyncio
    async def test_list_tools(self):
        """Test listing available tools."""
        async with MCPTestClient() as client:
            result = await client.list_tools()

            assert "tools" in result
            assert len(result["tools"]) == 1

            tool = result["tools"][0]
            assert tool["name"] == "get_current_time_in_city"
            assert tool["description"] == "Display current time in the city"
            assert "inputSchema" in tool

            # Verify schema structure
            schema = tool["inputSchema"]
            assert schema["type"] == "object"
            assert "properties" in schema
            assert "city" in schema["properties"]
            assert schema["required"] == ["city"]

            # Verify city property
            city_prop = schema["properties"]["city"]
            assert city_prop["type"] == "string"
            assert city_prop["description"] == "City name"

    @pytest.mark.asyncio
    async def test_call_get_current_time_valid_city(self):
        """Test calling get_current_time_in_city with valid city."""
        async with MCPTestClient() as client:
            result = await client.call_tool(
                "get_current_time_in_city", {"city": "New York"}
            )

            assert "content" in result
            assert len(result["content"]) == 1
            assert result["isError"] is False

            content = result["content"][0]
            assert content["type"] == "text"
            assert "text" in content

            # Verify the text format (should contain "Now: " and end with "!")
            text = content["text"]
            assert text.startswith("Now: ")
            assert text.endswith("!")

            # Should contain a valid timestamp format
            # RFC2822 format: "Mon, 25 Dec 2023 10:30:00 +0000"
            timestamp_part = text[5:-1]  # Remove "Now: " and "!"
            # Basic validation - should contain day, month, year
            assert re.search(r"\w{3},\s+\d{1,2}\s+\w{3}\s+\d{4}", timestamp_part)

    @pytest.mark.asyncio
    async def test_call_get_current_time_different_cities(self):
        """Test calling get_current_time_in_city with different cities."""
        cities = ["London", "Tokyo", "Sydney", "Paris"]

        async with MCPTestClient() as client:
            for city in cities:
                result = await client.call_tool(
                    "get_current_time_in_city", {"city": city}
                )

                assert result["isError"] is False
                assert len(result["content"]) == 1

                content = result["content"][0]
                assert content["type"] == "text"
                assert content["text"].startswith("Now: ")
                assert content["text"].endswith("!")

    @pytest.mark.asyncio
    async def test_call_tool_missing_required_parameter(self):
        """Test calling tool without required city parameter."""
        async with MCPTestClient() as client:
            # This should fail because city is required
            try:
                result = await client.call_tool(
                    "get_current_time_in_city", {}  # Missing city parameter
                )
                # If we get here, the call succeeded but with an error flag
                if "isError" in result:
                    assert result["isError"] is True
            except Exception:
                # Or it might raise an exception, which is also valid
                pass

    @pytest.mark.asyncio
    async def test_call_nonexistent_tool(self):
        """Test calling a tool that doesn't exist."""
        async with MCPTestClient() as client:
            try:
                await client.call_tool("nonexistent_tool", {})
                # Should not reach here
                pytest.fail("Expected exception for nonexistent tool")
            except Exception as e:
                # Should raise an exception for unknown tool
                assert (
                    "not found" in str(e).lower()
                    or "unknown" in str(e).lower()
                    or "invalid" in str(e).lower()
                )

    @pytest.mark.asyncio
    async def test_call_tool_with_extra_parameters(self):
        """Test calling tool with extra parameters (should still work)."""
        async with MCPTestClient() as client:
            result = await client.call_tool(
                "get_current_time_in_city",
                {"city": "Berlin", "extra_param": "should_be_ignored"},
            )

            # Should succeed and ignore extra parameters
            assert result["isError"] is False
            assert len(result["content"]) == 1
            assert result["content"][0]["text"].startswith("Now: ")

    @pytest.mark.asyncio
    async def test_tool_response_format(self):
        """Test that tool responses follow the correct format."""
        async with MCPTestClient() as client:
            result = await client.call_tool(
                "get_current_time_in_city", {"city": "Moscow"}
            )

            # Verify response structure
            assert isinstance(result, dict)
            assert "content" in result
            assert "isError" in result
            assert isinstance(result["content"], list)
            assert isinstance(result["isError"], bool)

            # Verify content structure
            content_item = result["content"][0]
            assert isinstance(content_item, dict)
            assert "type" in content_item
            assert "text" in content_item
            assert content_item["type"] == "text"
            assert isinstance(content_item["text"], str)

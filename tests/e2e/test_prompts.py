import pytest
from mcp_client_helper import MCPTestClient


class TestMCPPrompts:
    """Test MCP prompts functionality."""

    @pytest.mark.asyncio
    async def test_list_prompts(self):
        """Test listing available prompts."""
        async with MCPTestClient() as client:
            result = await client.list_prompts()

            assert "prompts" in result
            assert len(result["prompts"]) == 2

            # Check that we have the expected prompts
            prompt_names = [p["name"] for p in result["prompts"]]
            assert "current-time" in prompt_names
            assert "analyze-code" in prompt_names

            # Verify current-time prompt structure
            current_time_prompt = next(
                p for p in result["prompts"] if p["name"] == "current-time"
            )
            assert (
                current_time_prompt["description"] == "Display current time in the city"
            )
            assert "arguments" in current_time_prompt
            assert len(current_time_prompt["arguments"]) == 1

            city_arg = current_time_prompt["arguments"][0]
            assert city_arg["name"] == "city"
            assert city_arg["description"] == "City name"
            assert city_arg["required"] is True

            # Verify analyze-code prompt structure
            analyze_code_prompt = next(
                p for p in result["prompts"] if p["name"] == "analyze-code"
            )
            assert (
                analyze_code_prompt["description"]
                == "Analyze code for potential improvements"
            )
            assert "arguments" in analyze_code_prompt
            assert len(analyze_code_prompt["arguments"]) == 1

            lang_arg = analyze_code_prompt["arguments"][0]
            assert lang_arg["name"] == "language"
            assert lang_arg["description"] == "Programming language"
            assert lang_arg["required"] is True

    @pytest.mark.asyncio
    async def test_get_current_time_prompt(self):
        """Test getting the current-time prompt with arguments."""
        async with MCPTestClient() as client:
            result = await client.get_prompt("current-time", {"city": "Tokyo"})

            assert "description" in result
            assert result["description"] == "Display current time in the city"

            if result.get("messages"):
                # Verify message structure
                messages = result["messages"]
                assert isinstance(messages, list)
                assert len(messages) >= 1

                message = messages[0]
                assert "role" in message
                assert "content" in message
                assert message["role"] == "user"

                content = message["content"]
                assert "type" in content
                assert "text" in content
                assert content["type"] == "text"
                assert "city: Tokyo" in content["text"]

    @pytest.mark.asyncio
    async def test_get_analyze_code_prompt(self):
        """Test getting the analyze-code prompt with arguments."""
        async with MCPTestClient() as client:
            result = await client.get_prompt("analyze-code", {"language": "rust"})

            assert "description" in result
            assert result["description"] == "Analyze code for potential improvements"

            if result.get("messages"):
                # Verify message structure
                messages = result["messages"]
                assert isinstance(messages, list)
                assert len(messages) >= 1

                message = messages[0]
                assert "role" in message
                assert "content" in message
                assert message["role"] == "user"

                content = message["content"]
                assert "type" in content
                assert "text" in content
                assert content["type"] == "text"
                assert "language: rust" in content["text"]

    @pytest.mark.asyncio
    async def test_get_prompt_missing_arguments(self):
        """Test getting a prompt without required arguments."""
        async with MCPTestClient() as client:
            result = await client.get_prompt("current-time", {})

            # Should still return a result but with missing argument placeholder
            assert "description" in result

            if result.get("messages"):
                message = result["messages"][0]
                content = message["content"]
                assert "<missing>" in content["text"]

    @pytest.mark.asyncio
    async def test_get_nonexistent_prompt(self):
        """Test getting a prompt that doesn't exist."""
        async with MCPTestClient() as client:
            try:
                await client.get_prompt("nonexistent-prompt", {})
                pytest.fail("Expected exception for nonexistent prompt")
            except Exception as e:
                # Should raise an exception or return an error
                assert "not found" in str(e).lower() or "Prompt not found" in str(e)

    @pytest.mark.asyncio
    async def test_get_prompt_with_extra_arguments(self):
        """Test getting a prompt with extra arguments."""
        async with MCPTestClient() as client:
            result = await client.get_prompt(
                "current-time", {"city": "London", "extra_arg": "should_be_ignored"}
            )

            # Should succeed and use the valid arguments
            assert "description" in result
            assert result["description"] == "Display current time in the city"

    @pytest.mark.asyncio
    async def test_prompt_response_format(self):
        """Test that prompt responses follow the correct format."""
        async with MCPTestClient() as client:
            result = await client.get_prompt("current-time", {"city": "Paris"})

            # Verify response structure
            assert isinstance(result, dict)
            assert "description" in result
            assert isinstance(result["description"], str)

            if "messages" in result:
                assert isinstance(result["messages"], list)

                for message in result["messages"]:
                    assert isinstance(message, dict)
                    assert "role" in message
                    assert "content" in message
                    assert isinstance(message["role"], str)
                    assert isinstance(message["content"], dict)

                    content = message["content"]
                    assert "type" in content
                    assert "text" in content
                    assert isinstance(content["type"], str)
                    assert isinstance(content["text"], str)

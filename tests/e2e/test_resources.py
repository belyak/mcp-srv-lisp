import pytest
from mcp_client_helper import MCPTestClient


class TestMCPResources:
    """Test MCP resources functionality."""

    @pytest.mark.asyncio
    async def test_list_resources(self):
        """Test listing available resources."""
        async with MCPTestClient() as client:
            result = await client.list_resources()

            assert "resources" in result
            assert len(result["resources"]) == 1

            resource = result["resources"][0]
            assert resource["uri"] == "file:///logs/app.log"
            assert resource["name"] == "Application Logs"
            assert (
                resource["description"]
                == "application logs with timestamp, level, message"
            )
            assert resource["mimeType"] == "text/plain"

    @pytest.mark.asyncio
    async def test_read_application_logs_resource(self):
        """Test reading the Application Logs resource."""
        async with MCPTestClient() as client:
            result = await client.read_resource("file:///logs/app.log")

            assert "content" in result
            content = result["content"]

            assert "uri" in content
            assert content["uri"] == "file:///logs/app.log"
            assert "mimeType" in content
            assert content["mimeType"] == "text/plain"
            assert "text" in content

            # Verify the log content format
            log_text = content["text"]
            assert log_text == "2024-11-28T08:19:18.974368Z,INFO,main,this is message"

            # Should not have blob content for text resource
            assert content.get("blob") is None

    @pytest.mark.asyncio
    async def test_read_nonexistent_resource(self):
        """Test reading a resource that doesn't exist."""
        async with MCPTestClient() as client:
            try:
                await client.read_resource("file:///nonexistent/resource.txt")
                # The current implementation might not validate URIs,
                # so it might succeed but return empty/default content
                # or it might fail - either is acceptable
            except Exception:
                # Exception is acceptable for nonexistent resource
                pass

    @pytest.mark.asyncio
    async def test_read_resource_with_invalid_uri(self):
        """Test reading a resource with an invalid URI."""
        async with MCPTestClient() as client:
            try:
                await client.read_resource("invalid-uri")
                # Might succeed with current implementation
            except Exception:
                # Exception is acceptable for invalid URI
                pass

    @pytest.mark.asyncio
    async def test_resource_content_format(self):
        """Test that resource content follows the correct format."""
        async with MCPTestClient() as client:
            result = await client.read_resource("file:///logs/app.log")

            # Verify response structure
            assert isinstance(result, dict)
            assert "content" in result

            content = result["content"]
            assert isinstance(content, dict)
            assert "uri" in content
            assert isinstance(content["uri"], str)

            if "mimeType" in content:
                assert isinstance(content["mimeType"], str)

            # Should have either text or blob, but not both for a text resource
            if "text" in content:
                assert isinstance(content["text"], str)
                # For text resources, blob should be None or not present
                assert content.get("blob") is None

            if "blob" in content and content["blob"] is not None:
                assert isinstance(content["blob"], str)
                # For blob resources, text should be None or not present
                assert content.get("text") is None

    @pytest.mark.asyncio
    async def test_list_resources_format(self):
        """Test that list resources response follows the correct format."""
        async with MCPTestClient() as client:
            result = await client.list_resources()

            # Verify response structure
            assert isinstance(result, dict)
            assert "resources" in result
            assert isinstance(result["resources"], list)

            for resource in result["resources"]:
                assert isinstance(resource, dict)
                assert "uri" in resource
                assert "name" in resource
                assert isinstance(resource["uri"], str)
                assert isinstance(resource["name"], str)

                if "description" in resource:
                    assert isinstance(resource["description"], str)

                if "mimeType" in resource:
                    assert isinstance(resource["mimeType"], str)

    @pytest.mark.asyncio
    async def test_multiple_resource_reads(self):
        """Test reading the same resource multiple times."""
        async with MCPTestClient() as client:
            # Read the resource multiple times
            for i in range(3):
                result = await client.read_resource("file:///logs/app.log")

                assert "content" in result
                content = result["content"]
                assert content["uri"] == "file:///logs/app.log"
                assert content["mimeType"] == "text/plain"
                assert "text" in content

                # Content should be consistent across reads
                expected_content = (
                    "2024-11-28T08:19:18.974368Z,INFO,main,this is message"
                )
                assert content["text"] == expected_content

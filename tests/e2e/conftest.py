import asyncio
import subprocess
from pathlib import Path

import pytest


@pytest.fixture(scope="session")
def event_loop():
    """Create an instance of the default event loop for the test session."""
    loop = asyncio.get_event_loop_policy().new_event_loop()
    yield loop
    loop.close()


@pytest.fixture(scope="function")
async def mcp_server():
    """Start and manage MCP server process for testing."""
    # Use the Common Lisp binary
    project_root = Path(__file__).parent.parent.parent
    binary_path = project_root / "mcp-server"

    # Ensure the binary exists
    if not binary_path.exists():
        print(f"Building MCP server at {project_root}")
        build_result = subprocess.run(
            ["make", "build"],
            check=False,
            cwd=project_root,
            capture_output=True,
            text=True,
        )
        if build_result.returncode != 0:
            pytest.fail(f"Failed to build MCP server: {build_result.stderr}")

    # Start the server process
    process = subprocess.Popen(
        [str(binary_path), "--mcp"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=0,
    )

    # Give the server a moment to start
    await asyncio.sleep(0.1)

    # Check if process started successfully
    if process.poll() is not None:
        stdout, stderr = process.communicate()
        pytest.fail(f"MCP server failed to start. stdout: {stdout}, stderr: {stderr}")

    try:
        yield process
    finally:
        # Clean shutdown
        if process.poll() is None:
            try:
                process.terminate()
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()  # test comment

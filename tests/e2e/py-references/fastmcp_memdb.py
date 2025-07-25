#!/usr/bin/env python3
"""
A minimal FastMCP in-memory DB server with metadata.
Run with: pipx run fastmcp ./fastmcp_memdb.py
"""
from fastmcp import FastMCP

mcp = FastMCP(
    "memdb",
    version="1.0.0",
    instructions="A minimal in-memory key-value database MCP server.",
)

# In-memory DB
DB = {}


# Resource to list keys
@mcp.resource(
    uri="memory::/keys",
    name="list_keys",
    description="List all keys in the in-memory DB.",
)
async def list_keys():
    return {"keys": list(DB.keys()), "count": len(DB)}


# Resource to get a key
@mcp.resource(
    uri="memory::/get/{key}", name="get", description="Retrieve a value by key."
)
async def get(key: str):
    return {"key": key, "value": DB.get(key, None)}


# Action to set a key
@mcp.tool(
    name="set", description="Set a key-value pair."
)
async def set(key: str, value: str):
    DB[key] = value
    return {"status": "ok", "key": key, "value": value}


# Action to clear DB
@mcp.tool(name="clear", description="Clear all keys from DB.")
async def clear():
    DB.clear()
    return {"status": "cleared"}


if __name__ == "__main__":
    mcp.run()

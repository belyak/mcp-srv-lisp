#!/usr/bin/env python3
"""
A minimal FastMCP in-memory DB server with metadata.
Run with: pipx run fastmcp ./fastmcp_memdb.py
"""
from fastmcp import FastMCP, Context

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


@mcp.tool()
async def interpret_key(key: str, ctx: Context):
    """
    Interpret a key as a command using AI sampling.
    This function REQUIRES AI context and will fail without it.
    """

    if key in DB:
        # ONLY use AI interpretation - no fallback allowed
        try:
            interpretation = await ctx.sample(
                f"Explain the key '{key}'"
                f" with value '{DB[key]}' from the DB."
            )
            
            # Validate that we got a meaningful response
            if not interpretation:
                raise RuntimeError("AI sampling returned empty response")
            
            # Handle different response formats from FastMCP
            if isinstance(interpretation, str):
                if not interpretation.strip():
                    raise RuntimeError("AI sampling returned empty string")
                final_interpretation = interpretation
            elif hasattr(interpretation, 'text'):
                # FastMCP returns objects with .text attribute
                final_interpretation = interpretation.text
                if not final_interpretation.strip():
                    raise RuntimeError("AI sampling returned empty text in object")
            elif isinstance(interpretation, dict) and "text" in interpretation:
                if not interpretation["text"].strip():
                    raise RuntimeError("AI sampling returned empty text in dict")
                final_interpretation = interpretation["text"]
            else:
                # Convert to string but ensure it's meaningful
                final_interpretation = str(interpretation)
                if not final_interpretation.strip():
                    raise RuntimeError("AI sampling returned non-meaningful response")
            
            return {"status": "found", "key": key, 
                    "value": DB[key], 
                    "interpretation": final_interpretation}
                    
        except Exception as e:
            # Fail explicitly if AI context is not available
            raise RuntimeError(f"interpret_key requires AI sampling but failed: {e}")
    else:
        return {"status": "not found", "key": key}


# Action to clear DB
@mcp.tool(name="clear", description="Clear all keys from DB.")
async def clear():
    DB.clear()
    return {"status": "cleared"}


if __name__ == "__main__":
    mcp.run()

#include <iostream>
extern "C" {
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
}
// Error handling function
void report_errors(lua_State* L, int status) {
    if (status != LUA_OK) {
        std::cerr << "-- " << lua_tostring(L, -1) << std::endl;
        lua_pop(L, 1); // remove error message
    }
}

int main() {
    lua_State* L = luaL_newstate();  // Create a new Lua state
    luaL_openlibs(L);                // Load Lua libraries

    // Load and execute the main.lua script
    int status = luaL_loadfile(L, "main.lua");  // Adjust path to main.lua
    if (status == LUA_OK) {
        status = lua_pcall(L, 0, 0, 0);
    }

    // Report any errors
    report_errors(L, status);

    // Clean up and close the Lua state
    lua_close(L);

    return 0;
}

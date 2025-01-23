#include <iostream>
extern "C" {
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
int luaopen_socket_core(lua_State* L);  // Declare the LuaSocket open function

}

// Error handling function
void report_errors(lua_State* L, int status) {
    if (status != LUA_OK) {
        lua_getglobal(L, "debug");
        lua_getfield(L, -1, "traceback");
        lua_pushvalue(L, 1);
        lua_pushinteger(L, 2);
        lua_call(L, 2, 1);

        const char* msg = lua_tostring(L, -1);
        if (msg == NULL) {  // Is error object not a string?
            if (luaL_callmeta(L, -1, "__tostring") &&  // Does it have a metamethod
                lua_type(L, -1) == LUA_TSTRING) {  // that produces a string?
                msg = lua_tostring(L, -1);
            }
            else {
                msg = lua_pushfstring(L, "(error object is a %s value)", luaL_typename(L, -1));
            }
        }
        std::cerr << msg << std::endl;
        lua_pop(L, 2);  // Remove error message and traceback
    }
}

int main() {
    lua_State* L = luaL_newstate();  // Create a new Lua state
    luaL_openlibs(L);                // Load Lua libraries

    luaL_requiref(L, "luasocket", luaopen_socket_core, 1);
    lua_pop(L, 1);  // Remove the library from the stack
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

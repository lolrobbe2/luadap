project "sandbox"
    location "../src/sandbox"
    kind "ConsoleApp"
    language "C++"
    cppdialect "C++17"
    targetdir "bin/%{cfg.buildcfg}"
    objdir "bin-int/%{cfg.buildcfg}"

    files { "../src/sandbox/**.cpp", "../src/sandbox/**.h","../src/sandbox/**.lua" }

    includedirs {
        "../external/lua/src", -- Include directory for Lua
        "../external/luasocket/src" -- Include directory for LuaSocket
    }
    links { "LuaCore", "LuaSocket", "ws2_32" } -- Link with LuaCore and LuaSocket
    

    buildoptions { "/EHsc", "/utf-8", "/D _UNICODE", "/D UNICODE" }  -- Add flags for Debug configuration    
    
    filter "system:windows"
        systemversion "latest"
        defines {"SANDBOX_WINDOWS", "WIN32", "_WINDOWS", "_CRT_SECURE_NO_WARNINGS", "LUASOCKET_WINDOWS", "LUASOCKET_EXPORTS","LUA_STATICLIB" }

    filter "configurations:Debug"
        defines { "DEBUG" }
        symbols "On"
        debugdir "../src/sandbox"

    filter "configurations:Release"
        defines { "NDEBUG" }
        optimize "On"
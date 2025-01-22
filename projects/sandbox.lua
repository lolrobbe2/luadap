project "Sandbox"
    location "projects/sandbox"
    kind "ConsoleApp"
    language "C++"
    cppdialect "C++17"
    targetdir "bin/%{cfg.buildcfg}"
    objdir "bin-int/%{cfg.buildcfg}"

    files { "src/sandbox/**.cpp", "src/sandbox/**.h" }

    includedirs {
        "include",
        "external/lua",          -- Include directory for Lua
        "external/luasocket/src" -- Include directory for LuaSocket
    }

    links { "LuaCore", "LuaSocket" } -- Link with LuaCore and LuaSocket

    filter "system:windows"
        systemversion "latest"
        defines { "SANDBOX_WINDOWS" }

    filter "configurations:Debug"
        defines { "DEBUG" }
        symbols "On"

    filter "configurations:Release"
        defines { "NDEBUG" }
        optimize "On"

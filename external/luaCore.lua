project "LuaCore"
    location "external/lua"
    kind "StaticLib"
    language "C"
    targetdir "bin/%{cfg.buildcfg}"
    objdir "bin-int/%{cfg.buildcfg}"

    files { "lua/**.c", "lua/**.h" }

    includedirs { "lua/src" }

    filter "system:windows"
        systemversion "latest"
        defines { "LUA_CORE_WINDOWS" }

    filter "configurations:Debug"
        defines { "DEBUG" }
        symbols "On"

    filter "configurations:Release"
        defines { "NDEBUG" }
        optimize "On"

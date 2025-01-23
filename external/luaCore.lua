project "luacore"
    location "external/lua"
    kind "StaticLib"
    language "C"
    targetdir "bin/%{cfg.buildcfg}"
    objdir "bin-int/%{cfg.buildcfg}"

    files { "../external/lua/src/**.c", "../external/lua/src/**.h" }

    includedirs { "../external/lua/src" }
    excludes 
    {
        "../external/lua/src/lua.c",
        "../external/lua/src/luac.c",
        "../external/lua/src/print.c"
    }
    filter "system:windows"
        systemversion "latest"
        defines { "LUA_CORE_WINDOWS" }

    filter "configurations:Debug"
        defines { "DEBUG" }
        symbols "On"

    filter "configurations:Release"
        defines { "NDEBUG" }
        optimize "On"

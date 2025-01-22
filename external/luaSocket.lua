project "LuaSocket"
    location "external/luasocket"
    kind "SharedLib"
    language "C"
    targetdir "bin/%{cfg.buildcfg}"
    objdir "bin-int/%{cfg.buildcfg}"

    files { "luasocket/src/**.c", "luasocket/src/**.h" }

    includedirs { "lua/src", "luasocket/src" }

    filter "system:windows"
        systemversion "latest"
        defines { "LUASOCKET_WINDOWS" }

    filter "configurations:Debug"
        defines { "DEBUG" }
        symbols "On"

    filter "configurations:Release"
        defines { "NDEBUG" }
        optimize "On"

project "LuaSocket"
    location "external/luasocket"
    kind "StaticLib"
    language "C"
    targetdir "bin/%{cfg.buildcfg}"
    objdir "bin-int/%{cfg.buildcfg}"

    files { "external/luasocket/src/**.c", "external/luasocket/src/**.h" }

    includedirs { "external/lua/src", "external/luasocket/src" }

    filter "system:windows"
        systemversion "latest"
        defines { "LUASOCKET_WINDOWS" }

    filter "configurations:Debug"
        defines { "DEBUG" }
        symbols "On"

    filter "configurations:Release"
        defines { "NDEBUG" }
        optimize "On"

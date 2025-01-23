project "luasocket"
    location "external/luasocket"
    kind "SharedLib"  -- Ensure LuaSocket is built as a shared library
    language "C"
    targetdir "bin/%{cfg.buildcfg}"
    objdir "bin-int/%{cfg.buildcfg}"

    files {
        "luasocket/src/auxiliar.c", "luasocket/src/auxiliar.h",
        "luasocket/src/buffer.c", "luasocket/src/buffer.h",
        "luasocket/src/compat.c", "luasocket/src/compat.h",
        "luasocket/src/except.c", "luasocket/src/except.h",
        "luasocket/src/inet.c", "luasocket/src/inet.h",
        "luasocket/src/io.c", "luasocket/src/io.h",
        "luasocket/src/luasocket.c", "luasocket/src/luasocket.h",
        "luasocket/src/mime.c", "luasocket/src/mime.h",
        "luasocket/src/options.c", "luasocket/src/options.h",
        "luasocket/src/select.c", "luasocket/src/select.h",
        "luasocket/src/tcp.c", "luasocket/src/tcp.h",
        "luasocket/src/timeout.c", "luasocket/src/timeout.h",
        "luasocket/src/udp.c", "luasocket/src/udp.h",
    }

    includedirs { "lua/src", "luasocket/src" }  -- Adjust include paths to be relative

    links { "LuaCore" }  -- Link LuaCore

    filter "system:windows"
        files { "luasocket/src/wsocket.c", "luasocket/src/wsocket.h" }
        systemversion "latest"
        defines { "WIN32", "_WINDOWS", "_CRT_SECURE_NO_WARNINGS", "LUASOCKET_WINDOWS", "LUASOCKET_EXPORTS" }
        links { "ws2_32" }  -- Link Windows Sockets library

    filter "system:linux or macosx"
        files {
            "luasocket/src/usocket.c", "luasocket/src/usocket.h",  -- Include Unix-specific files for Linux or macOS
            "luasocket/src/unix.c", "luasocket/src/unix.h",
            "luasocket/src/unixdgram.c", "luasocket/src/unixdgram.h",
            "luasocket/src/unixstream.c", "luasocket/src/unixstream.h"
        }
        defines { "LUA_USE_POSIX", "LUA_USE_DLOPEN" }  -- Define Unix-specific macros

    filter "configurations:Debug"
        defines { "DEBUG" }
        symbols "On"

    filter "configurations:Release"
        defines { "NDEBUG" }
        optimize "On"

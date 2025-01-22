workspace "MyProject"
    architecture "x86_64"
    configurations { "Debug", "Release" }

    -- Include projects
    include "projects/sandbox.lua"
    group "core"
    include "external/luaCore.lua"
    include "external/luaSocket.lua"
    group ""

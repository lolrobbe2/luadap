workspace "MyProject"
    architecture "x86_64"
    configurations { "Debug", "Release" }

    -- Include projects
    include "projects/sandbox.lua"
    group "core"
    include "projects/luaCore.lua"
    include "projects/luaSocket.lua"
    group ""

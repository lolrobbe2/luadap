local luadap = require("luadap")

-- Start the Luadap server

-- Main loop to accept client connections and handle data

luadap.start()

function fibonacci()
    local a, b = 0, 1
    local test = {x="1",y="z"}
    print_hello()
    while true do
        print("fib:" .. a)
        a, b = b, a + b
    end
end

function print_hello()
    local test = { "test1", "test12", "test123" }
    print("Hello, World!")
end
-- Start the endless Fibonacci sequence
local c,d = 20,6
print_hello()
fibonacci()

local luadap = require("luadap")

-- Start the Luadap server

-- Main loop to accept client connections and handle data

luadap.start()

function fibonacci()
    local a, b = 0, 1
    while true do
        print("fib:" .. a)
        a, b = b, a + b
    end
end

function print_hello()
    print("Hello, World!")
end
-- Start the endless Fibonacci sequence
local test = {"test1", "test12","test123"}
print_hello()
fibonacci()

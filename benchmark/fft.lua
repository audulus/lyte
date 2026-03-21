-- FFT benchmark
-- 1024-point radix-2 FFT, 2000 iterations

local N = 1024
local ITERS = 2000
local LOG2N = 10
local sin = math.sin
local cos = math.cos
local pi = math.pi
local floor = math.floor

-- Bit-reversal using arithmetic (compatible with Lua 5.1 / LuaJIT)
local function bit_reverse(x, bits)
    local result = 0
    for i = 1, bits do
        result = result * 2 + (x % 2)
        x = floor(x / 2)
    end
    return result
end

local function fft(re, im, n)
    -- Bit-reversal permutation
    for i = 0, n - 1 do
        local j = bit_reverse(i, LOG2N)
        if i < j then
            re[i], re[j] = re[j], re[i]
            im[i], im[j] = im[j], im[i]
        end
    end

    -- Butterfly stages
    local size = 2
    while size <= n do
        local half = math.floor(size / 2)
        local angle = -2.0 * pi / size

        for group = 0, n - 1, size do
            for k = 0, half - 1 do
                local theta = angle * k
                local wr = cos(theta)
                local wi = sin(theta)

                local i = group + k
                local j = i + half

                local tr = wr * re[j] - wi * im[j]
                local ti = wr * im[j] + wi * re[j]

                re[j] = re[i] - tr
                im[j] = im[i] - ti
                re[i] = re[i] + tr
                im[i] = im[i] + ti
            end
        end
        size = size * 2
    end
end

local function run()
    local re = {}
    local im = {}
    local checksum = 0.0

    for iter = 0, ITERS - 1 do
        local freq1 = 50.0
        local freq2 = 120.0
        for i = 0, N - 1 do
            local t = i / N
            re[i] = sin(2.0 * pi * freq1 * t) + 0.5 * sin(2.0 * pi * freq2 * t)
            im[i] = 0.0
        end
        fft(re, im, N)
        checksum = checksum + re[0]
    end
end

local t0 = os.clock()
run()
local elapsed = os.clock() - t0
io.stderr:write(string.format("exec: %.3fs\n", elapsed))

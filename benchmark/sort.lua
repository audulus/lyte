-- Sorting benchmark
-- Quicksort 10000 elements, 50 iterations

local N = 10000
local ITERS = 50

local function lcg(state)
    return (state * 1103 + 12345) % 1000000
end

local function quicksort(a, lo, hi)
    if lo < hi then
        local pivot_val = a[hi]
        local i = lo
        for j = lo, hi - 1 do
            if a[j] < pivot_val then
                a[i], a[j] = a[j], a[i]
                i = i + 1
            end
        end
        a[i], a[hi] = a[hi], a[i]
        quicksort(a, lo, i - 1)
        quicksort(a, i + 1, hi)
    end
end

local function run()
    local a = {}
    local checksum = 0

    for iter = 0, ITERS - 1 do
        local seed = iter * 7 + 42
        for i = 1, N do
            seed = lcg(seed)
            a[i] = seed
        end
        quicksort(a, 1, N)
        checksum = checksum + a[1] + a[N]
    end
end

local t0 = os.clock()
run()
local elapsed = os.clock() - t0
io.stderr:write(string.format("exec: %.3fs\n", elapsed))

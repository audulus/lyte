-- Biquad filter DSP benchmark
-- Process 10 million samples through a lowpass filter

local sin = math.sin
local pi = math.pi

-- Design a 2nd-order lowpass (RBJ cookbook)
local function lpf(fc, fs, q)
    local w0 = 2.0 * pi * fc / fs
    local alpha = sin(w0) / (2.0 * q)
    local cs = math.cos(w0)

    local a0 = 1.0 + alpha
    local inv = 1.0 / a0

    return {
        b0 = ((1.0 - cs) / 2.0) * inv,
        b1 = (1.0 - cs) * inv,
        b2 = ((1.0 - cs) / 2.0) * inv,
        a1 = (-2.0 * cs) * inv,
        a2 = (1.0 - alpha) * inv,
        x1 = 0.0, x2 = 0.0,
        y1 = 0.0, y2 = 0.0
    }
end

local function run()
    local bq = lpf(1000.0, 44100.0, 0.707)

    local n = 10000000
    local phase = 0.0
    local freq = 440.0 / 44100.0
    local last_y = 0.0
    local two_pi = 2.0 * pi

    -- Process a 440 Hz sine wave through the filter
    for i = 1, n do
        local x = sin(phase * two_pi)
        phase = phase + freq
        if phase > 1.0 then phase = phase - 1.0 end

        local y = bq.b0 * x + bq.b1 * bq.x1 + bq.b2 * bq.x2 - bq.a1 * bq.y1 - bq.a2 * bq.y2
        bq.x2 = bq.x1
        bq.x1 = x
        bq.y2 = bq.y1
        bq.y1 = y
        last_y = y
    end
end

local t0 = os.clock()
run()
local elapsed = os.clock() - t0
io.stderr:write(string.format("exec: %.3fs\n", elapsed))

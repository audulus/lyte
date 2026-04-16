-- Sallen-Key low-pass filter benchmark
-- Process 2 million samples with per-sample coefficient recomputation
-- Based on Andrew Simper's nodal filter equations (SkfLinearTrapOptimised2.pdf)

local sin = math.sin
local exp = math.exp
local tan = math.tan
local log = math.log
local pi  = math.pi

local function run()
    local n = 2000000
    local inv_sr = 1.0 / 44100.0
    local f_min = log(50.0)
    local f_max = log(16000.0)
    local f_range = f_max - f_min

    local ic1eq = 0.0
    local ic2eq = 0.0
    local last_y = 0.0

    local phase = 0.0
    local freq = 440.0 / 44100.0
    local two_pi = 2.0 * pi

    local fc_val = 0.5
    local res_val = 0.3

    for i = 1, n do
        -- generate input sample
        local x = sin(phase * two_pi)
        phase = phase + freq
        if phase > 1.0 then phase = phase - 1.0 end

        -- clamp controls (modulate fc slightly so coefficients vary per sample)
        local fc_norm = fc_val + 0.001 * x
        if fc_norm < 0.0 then fc_norm = 0.0 end
        if fc_norm > 1.0 then fc_norm = 1.0 end
        local res_c = res_val
        if res_c < 0.0 then res_c = 0.0 end
        if res_c > 1.0 then res_c = 1.0 end

        -- map normalised fc to log frequency and compute coefficients
        local cutoff = exp(fc_norm * f_range + f_min)
        local g  = tan(pi * cutoff * inv_sr)
        local k  = 2.0 * res_c
        local g1 = 1.0 + g
        local a0 = 1.0 / (g1 * g1 - g * k)
        local a1 = k * a0
        local a2 = g1 * a0
        local a3 = g * a2
        local a4 = 1.0 / g1
        local a5 = g * a4

        -- trapezoidal integrators
        local v1 = a1 * ic2eq + a2 * ic1eq + a3 * x
        local v2 = a4 * ic2eq + a5 * v1

        ic1eq = 2.0 * (v1 - k * v2) - ic1eq
        ic2eq = 2.0 * v2 - ic2eq

        last_y = v2
    end
end

local t0 = os.clock()
run()
local elapsed = os.clock() - t0
io.stderr:write(string.format("exec: %.3fs\n", elapsed))

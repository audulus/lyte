// Sallen-Key low-pass filter benchmark
// Process 2 million samples with per-sample coefficient recomputation
// Based on Andrew Simper's nodal filter equations (SkfLinearTrapOptimised2.pdf)

#include <math.h>
#include <stdio.h>
#include <time.h>

int main(void) {
    int n = 2000000;
    double inv_sr = 1.0 / 44100.0;
    double f_min = log(50.0);
    double f_max = log(16000.0);
    double f_range = f_max - f_min;

    double ic1eq = 0.0;
    double ic2eq = 0.0;
    double last_y = 0.0;

    double phase = 0.0;
    double freq = 440.0 / 44100.0;
    double two_pi = 2.0 * M_PI;

    double fc_val = 0.5;
    double res_val = 0.3;

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    for (int i = 0; i < n; i++) {
        double x = sin(phase * two_pi);
        phase += freq;
        if (phase > 1.0) phase -= 1.0;

        double fc_norm = fc_val + 0.001 * x;
        if (fc_norm < 0.0) fc_norm = 0.0;
        if (fc_norm > 1.0) fc_norm = 1.0;
        double res_c = res_val;
        if (res_c < 0.0) res_c = 0.0;
        if (res_c > 1.0) res_c = 1.0;

        double cutoff = exp(fc_norm * f_range + f_min);
        double g  = tan(M_PI * cutoff * inv_sr);
        double k  = 2.0 * res_c;
        double g1 = 1.0 + g;
        double a0 = 1.0 / (g1 * g1 - g * k);
        double a1 = k * a0;
        double a2 = g1 * a0;
        double a3 = g * a2;
        double a4 = 1.0 / g1;
        double a5 = g * a4;

        double v1 = a1 * ic2eq + a2 * ic1eq + a3 * x;
        double v2 = a4 * ic2eq + a5 * v1;

        ic1eq = 2.0 * (v1 - k * v2) - ic1eq;
        ic2eq = 2.0 * v2 - ic2eq;

        last_y = v2;
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double elapsed = (t1.tv_sec - t0.tv_sec) + (t1.tv_nsec - t0.tv_nsec) * 1e-9;
    fprintf(stderr, "exec: %.3fs\n", elapsed);
    printf("%d samples, last_y=%f\n", n, last_y);
    return 0;
}

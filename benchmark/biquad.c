#include <stdio.h>
#include <math.h>
#include <time.h>

typedef struct {
    float b0, b1, b2;
    float a1, a2;
    float x1, x2;
    float y1, y2;
} Biquad;

static Biquad lpf(float fc, float fs, float q) {
    float w0 = 2.0f * 3.14159265f * fc / fs;
    float alpha = sinf(w0) / (2.0f * q);
    float cs = cosf(w0);

    float a0 = 1.0f + alpha;
    float inv = 1.0f / a0;

    Biquad bq;
    bq.b1 = (1.0f - cs) * inv;
    bq.b0 = bq.b1 / 2.0f;
    bq.b2 = bq.b0;
    bq.a1 = (0.0f - 2.0f * cs) * inv;
    bq.a2 = (1.0f - alpha) * inv;
    bq.x1 = 0.0f;
    bq.x2 = 0.0f;
    bq.y1 = 0.0f;
    bq.y2 = 0.0f;
    return bq;
}

int main(void) {
    Biquad bq = lpf(1000.0f, 44100.0f, 0.707f);

    int n = 10000000;
    float phase = 0.0f;
    float freq = 440.0f / 44100.0f;
    float two_pi = 2.0f * 3.14159265f;

    clock_t t0 = clock();

    for (int i = 0; i < n; i++) {
        float x = sinf(phase * two_pi);
        phase = phase + freq;
        if (phase > 1.0f) { phase = phase - 1.0f; }

        float y = bq.b0 * x + bq.b1 * bq.x1 + bq.b2 * bq.x2
                - bq.a1 * bq.y1 - bq.a2 * bq.y2;
        bq.x2 = bq.x1;
        bq.x1 = x;
        bq.y2 = bq.y1;
        bq.y1 = y;
    }

    double elapsed = (double)(clock() - t0) / CLOCKS_PER_SEC;
    fprintf(stderr, "exec: %.3fs\n", elapsed);

    // Prevent the compiler from optimizing away the loop.
    volatile float sink = bq.y1;
    (void)sink;
    return 0;
}

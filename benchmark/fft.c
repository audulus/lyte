#include <stdio.h>
#include <math.h>
#include <time.h>

#define N 1024
#define ITERS 2000
#define LOG2N 10

static int bit_reverse(int x, int bits) {
    int result = 0;
    for (int i = 0; i < bits; i++) {
        result = result * 2 + (x & 1);
        x >>= 1;
    }
    return result;
}

static void fft(float *re, float *im, int n) {
    // Bit-reversal permutation
    for (int i = 0; i < n; i++) {
        int j = bit_reverse(i, LOG2N);
        if (i < j) {
            float tr = re[i]; re[i] = re[j]; re[j] = tr;
            float ti = im[i]; im[i] = im[j]; im[j] = ti;
        }
    }

    // Butterfly stages
    for (int size = 2; size <= n; size *= 2) {
        int half = size / 2;
        float angle = -2.0f * 3.14159265f / (float)size;

        for (int group = 0; group < n; group += size) {
            for (int k = 0; k < half; k++) {
                float theta = angle * (float)k;
                float wr = cosf(theta);
                float wi = sinf(theta);

                int i = group + k;
                int j = i + half;

                float tr = wr * re[j] - wi * im[j];
                float ti = wr * im[j] + wi * re[j];

                re[j] = re[i] - tr;
                im[j] = im[i] - ti;
                re[i] = re[i] + tr;
                im[i] = im[i] + ti;
            }
        }
    }
}

int main(void) {
    float re[N], im[N];
    float checksum = 0.0f;

    clock_t t0 = clock();

    for (int iter = 0; iter < ITERS; iter++) {
        float freq1 = 50.0f;
        float freq2 = 120.0f;
        for (int i = 0; i < N; i++) {
            float t = (float)i / (float)N;
            re[i] = sinf(2.0f * 3.14159265f * freq1 * t)
                   + 0.5f * sinf(2.0f * 3.14159265f * freq2 * t);
            im[i] = 0.0f;
        }
        fft(re, im, N);
        checksum += re[0];
    }

    double elapsed = (double)(clock() - t0) / CLOCKS_PER_SEC;
    fprintf(stderr, "exec: %.3fs\n", elapsed);

    volatile float sink = checksum;
    (void)sink;
    return 0;
}

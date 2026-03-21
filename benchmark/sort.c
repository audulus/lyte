#include <stdio.h>
#include <time.h>

#define N 10000
#define ITERS 50

static int lcg(int state) {
    return (state * 1103 + 12345) % 1000000;
}

static void swap(int *a, int *b) {
    int t = *a; *a = *b; *b = t;
}

static void quicksort(int *a, int lo, int hi) {
    if (lo < hi) {
        int pivot_val = a[hi];
        int i = lo;
        for (int j = lo; j < hi; j++) {
            if (a[j] < pivot_val) {
                swap(&a[i], &a[j]);
                i++;
            }
        }
        swap(&a[i], &a[hi]);
        quicksort(a, lo, i - 1);
        quicksort(a, i + 1, hi);
    }
}

int main(void) {
    int a[N];
    int checksum = 0;

    clock_t t0 = clock();

    for (int iter = 0; iter < ITERS; iter++) {
        int seed = iter * 7 + 42;
        for (int i = 0; i < N; i++) {
            seed = lcg(seed);
            a[i] = seed;
        }
        quicksort(a, 0, N - 1);
        checksum += a[0] + a[N - 1];
    }

    double elapsed = (double)(clock() - t0) / CLOCKS_PER_SEC;
    fprintf(stderr, "exec: %.3fs\n", elapsed);

    volatile int sink = checksum;
    (void)sink;
    return 0;
}

// FFI performance harness for DSP scripts with init/process entry points.
//
// Drives lyte_compiler_compile (the codepath used by the xcframework, which
// the CLI does NOT touch), binds every [f32] slice port to a host buffer,
// times init() once and process() ITERATIONS times. Reports a single line:
//
//     compile=<seconds>s  exec=<seconds>s
//
// The wrapper benchmark/run-freeverb.sh builds liblyte.dylib + this harness
// and averages over N runs.
//
// The whole .lyte file is added via lyte_compiler_add_prelude so that
// `assume` statements (used to discharge bounds checks on slice ports) are
// accepted, mirroring how the xcframework hosts trusted DSP code.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <time.h>
#include "lyte.h"

#ifndef MAX_FRAMES
#define MAX_FRAMES 256
#endif
#define MAX_SLICE_PORTS 64
#define STORAGE_LEN 16

static bool send_stub(void* ctx, void* buf_data, int32_t buf_len) {
    (void)ctx; (void)buf_data; (void)buf_len;
    return false;
}

static char* slurp(const char* path) {
    FILE* f = fopen(path, "rb");
    if (!f) { perror(path); return NULL; }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    char* buf = (char*)malloc((size_t)sz + 1);
    if (!buf) { fclose(f); return NULL; }
    if (fread(buf, 1, (size_t)sz, f) != (size_t)sz) {
        perror("fread"); free(buf); fclose(f); return NULL;
    }
    buf[sz] = 0;
    fclose(f);
    return buf;
}

static double now_seconds(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec * 1e-9;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s <file.lyte>\n", argv[0]);
        return 1;
    }
    const char* path = argv[1];

    long iterations = 10000;
    const char* iter_env = getenv("FREEVERB_ITERATIONS");
    if (iter_env && *iter_env) {
        iterations = strtol(iter_env, NULL, 10);
        if (iterations <= 0) iterations = 10000;
    }

    char* source = slurp(path);
    if (!source) return 1;

    const char* entry_names[] = {"init", "process"};
    LyteCompiler* compiler = lyte_compiler_new(entry_names, 2);
    if (!compiler) { fprintf(stderr, "lyte_compiler_new failed\n"); return 1; }

    if (!lyte_compiler_add_prelude(compiler, source)) {
        fprintf(stderr, "parse error: %s\n", lyte_compiler_get_error(compiler));
        return 1;
    }

    double tc0 = now_seconds();
    LyteProgram* program = lyte_compiler_compile(compiler);
    double tc1 = now_seconds();
    if (!program) {
        fprintf(stderr, "compile error: %s\n", lyte_compiler_get_error(compiler));
        return 1;
    }

    size_t globals_size = lyte_program_get_globals_size(program);
    uint8_t* globals = lyte_globals_alloc(program);
    if (!globals) { fprintf(stderr, "globals alloc failed\n"); return 1; }

    static float port_bufs[MAX_SLICE_PORTS][MAX_FRAMES];
    static float storage_buf[STORAGE_LEN];
    int port_idx = 0;

    int32_t frames_value = MAX_FRAMES;
    float   sample_rate_value = 44100.0f;

    size_t n = lyte_program_get_globals_count(program);
    for (size_t i = 0; i < n; i++) {
        const char* name = lyte_program_get_global_name(program, i);
        size_t off = lyte_program_get_global_offset(program, i);
        const char* ty = lyte_program_get_global_type(program, i);
        bool is_extern = lyte_program_get_global_is_extern(program, i);

        if (is_extern) {
            // Host scripts in the wild declare extern hooks like `send`. We
            // bind a no-op stub for any extern so codegen finds a symbol;
            // performance scripts are not expected to actually call them.
            lyte_globals_bind_extern(globals, off, (const void*)send_stub, NULL);
            continue;
        }
        if (strcmp(name, "frames") == 0) {
            memcpy(globals + off, &frames_value, sizeof(int32_t));
            continue;
        }
        if (strcmp(name, "sampleRate") == 0) {
            memcpy(globals + off, &sample_rate_value, sizeof(float));
            continue;
        }
        if (strcmp(ty, "[f32]") == 0) {
            if (strcmp(name, "storage") == 0) {
                lyte_globals_bind_slice(globals, off, storage_buf, STORAGE_LEN);
            } else {
                if (port_idx >= MAX_SLICE_PORTS) {
                    fprintf(stderr, "too many slice ports (>%d)\n", MAX_SLICE_PORTS);
                    return 1;
                }
                lyte_globals_bind_slice(globals, off, port_bufs[port_idx++], MAX_FRAMES);
            }
        }
    }

    // Drive any host gating ports so process() runs the full DSP body. Some
    // scripts early-out when active/CH_ON are zero; populate both if present.
    for (size_t i = 0; i < n; i++) {
        const char* name = lyte_program_get_global_name(program, i);
        if (strcmp(name, "active") == 0 || strcmp(name, "CH_ON") == 0) {
            void* slice_ptr;
            memcpy(&slice_ptr, globals + lyte_program_get_global_offset(program, i),
                   sizeof(void*));
            if (slice_ptr) ((float*)slice_ptr)[0] = 1.0f;
        }
    }

    if (!lyte_entry_point_call(program, 0, globals)) {
        fprintf(stderr, "init failed: %s\n", lyte_compiler_get_error(compiler));
        return 1;
    }

    double t0 = now_seconds();
    for (long i = 0; i < iterations; i++) {
        if (!lyte_entry_point_call(program, 1, globals)) {
            fprintf(stderr, "process failed at iteration %ld: %s\n",
                    i, lyte_compiler_get_error(compiler));
            return 1;
        }
    }
    double t1 = now_seconds();

    printf("compile=%.3fs  exec=%.3fs  (%ld * process() at %d frames, globals=%zu bytes)\n",
           tc1 - tc0, t1 - t0, iterations, MAX_FRAMES, globals_size);

    lyte_globals_free(globals, globals_size);
    lyte_program_free(program);
    lyte_compiler_free(compiler);
    free(source);
    return 0;
}

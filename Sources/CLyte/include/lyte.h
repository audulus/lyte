#ifndef LYTE_H
#define LYTE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/// Opaque compiler handle.
typedef struct LyteCompiler LyteCompiler;

/// Create a new compiler instance.
LyteCompiler* lyte_compiler_new(void);

/// Free a compiler instance.
void lyte_compiler_free(LyteCompiler* compiler);

/// Get the last error message, or NULL if no error.
const char* lyte_compiler_get_error(const LyteCompiler* compiler);

/// Parse source code. Returns true on success.
bool lyte_compiler_parse(LyteCompiler* compiler, const char* source, const char* filename);

/// Type-check the parsed source. Returns true on success.
bool lyte_compiler_check(LyteCompiler* compiler);

/// Monomorphize generics. Returns true on success.
bool lyte_compiler_specialize(LyteCompiler* compiler);

/// JIT compile. Returns true on success.
bool lyte_compiler_jit(LyteCompiler* compiler);

/// Get the JIT-compiled code pointer.
/// The function signature is: void main(uint8_t* globals)
/// Returns NULL if JIT has not been run or failed.
const uint8_t* lyte_compiler_get_code_ptr(const LyteCompiler* compiler);

/// Get the size in bytes of the globals buffer needed by the JIT-compiled code.
/// Returns 0 if JIT has not been run or failed.
size_t lyte_compiler_get_globals_size(const LyteCompiler* compiler);

/// Convenience: parse, check, specialize, and JIT compile in one call.
/// Returns true on success.
bool lyte_compile(LyteCompiler* compiler, const char* source, const char* filename);

#ifdef __cplusplus
}
#endif

#endif // LYTE_H

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

/// Get the number of global variables. Available after JIT compilation.
/// Returns 0 if JIT has not been run.
size_t lyte_compiler_get_globals_count(const LyteCompiler* compiler);

/// Get the name of a global variable by index.
/// Returns NULL if index is out of bounds.
const char* lyte_compiler_get_global_name(const LyteCompiler* compiler, size_t index);

/// Get the byte offset of a global variable within the globals buffer.
/// Returns 0 if index is out of bounds.
size_t lyte_compiler_get_global_offset(const LyteCompiler* compiler, size_t index);

/// Get the size in bytes of a global variable.
/// Returns 0 if index is out of bounds.
size_t lyte_compiler_get_global_size(const LyteCompiler* compiler, size_t index);

/// Get the type of a global variable as a string in lyte syntax (e.g. "f32", "[i32; 4]").
/// Returns NULL if index is out of bounds.
const char* lyte_compiler_get_global_type(const LyteCompiler* compiler, size_t index);

// ============ VM API ============

/// Compile to VM bytecode. Returns true on success.
/// Must call parse, check, and specialize first.
bool lyte_compiler_compile_vm(LyteCompiler* compiler);

/// Run the VM program. Returns true on success.
/// Must call lyte_compiler_compile_vm first.
/// After running, globals can be read via lyte_compiler_get_vm_globals_ptr.
bool lyte_compiler_run_vm(LyteCompiler* compiler);

/// Get a pointer to the VM's globals buffer.
/// Returns NULL if the VM has not been run.
/// The pointer is valid until the next call to lyte_compiler_run_vm
/// or lyte_compiler_free.
uint8_t* lyte_compiler_get_vm_globals_ptr(LyteCompiler* compiler);

/// Get the size in bytes of the VM globals buffer.
/// Returns 0 if no VM program has been compiled.
size_t lyte_compiler_get_vm_globals_size(const LyteCompiler* compiler);

/// Convenience: parse, check, specialize, and compile to VM in one call.
/// Returns true on success.
bool lyte_compile_vm(LyteCompiler* compiler, const char* source, const char* filename);

#ifdef __cplusplus
}
#endif

#endif // LYTE_H

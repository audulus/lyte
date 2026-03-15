#ifndef LYTE_H
#define LYTE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// ============ Opaque handles ============

typedef struct LyteCompiler LyteCompiler;
typedef struct LyteProgram LyteProgram;
typedef struct LyteEntryPoint LyteEntryPoint;

/// Cancel callback type. Return true to cancel execution.
typedef bool (*lyte_cancel_fn)(void* user_data);

// ============ Compiler ============

/// Create a new compiler instance.
LyteCompiler* lyte_compiler_new(void);

/// Free a compiler instance.
void lyte_compiler_free(LyteCompiler* compiler);

/// Returns true if an internal compiler error (panic) occurred.
bool lyte_compiler_had_ice(const LyteCompiler* compiler);

/// Get the last error message, or NULL if no error.
const char* lyte_compiler_get_error(const LyteCompiler* compiler);

/// Add source code to the compiler. May be called multiple times
/// to add multiple files. Returns true on success.
bool lyte_compiler_add_source(LyteCompiler* compiler, const char* source, const char* filename);

/// Set entry point function names. If not called, defaults to ["main"].
/// Must be called before lyte_compiler_compile.
bool lyte_compiler_set_entry_points(LyteCompiler* compiler, const char** names, size_t count);

/// Parse, type-check, specialize, and compile all added source into
/// a LyteProgram (auto-selects JIT or VM backend).
/// Returns NULL on error. Caller must free with lyte_program_free.
LyteProgram* lyte_compiler_compile(LyteCompiler* compiler);

// ============ Program ============

/// Free a compiled program and all its entry points.
void lyte_program_free(LyteProgram* program);

/// Get the size in bytes of the globals buffer.
size_t lyte_program_get_globals_size(const LyteProgram* program);

/// Get the number of global variables.
size_t lyte_program_get_globals_count(const LyteProgram* program);

/// Get the name of a global variable by index.
const char* lyte_program_get_global_name(const LyteProgram* program, size_t index);

/// Get the byte offset of a global variable within the globals buffer.
size_t lyte_program_get_global_offset(const LyteProgram* program, size_t index);

/// Get the size in bytes of a global variable.
size_t lyte_program_get_global_size(const LyteProgram* program, size_t index);

/// Get the type of a global variable as a string.
const char* lyte_program_get_global_type(const LyteProgram* program, size_t index);

/// Look up an entry point by name. Returns NULL if not found.
/// The returned handle is owned by the program; do NOT free it separately.
const LyteEntryPoint* lyte_program_get_entry_point(const LyteProgram* program, const char* name);

/// Set a cancel callback. Called approximately every 1024 backward jumps.
/// If it returns true, execution is cancelled. Pass NULL to disable.
void lyte_program_set_cancel_callback(LyteProgram* program, lyte_cancel_fn callback, void* user_data);

// ============ Entry point invocation ============

/// Call an entry point with an external globals buffer.
/// The buffer must be at least lyte_program_get_globals_size() bytes.
/// Returns true on success, false if cancelled or error.
bool lyte_entry_point_call(const LyteEntryPoint* entry, uint8_t* globals);

// ============ Globals helpers ============

/// Allocate a zeroed globals buffer of the correct size.
/// Caller must free with lyte_globals_free().
uint8_t* lyte_globals_alloc(const LyteProgram* program);

/// Free a globals buffer allocated by lyte_globals_alloc.
void lyte_globals_free(uint8_t* globals, size_t size);

#ifdef __cplusplus
}
#endif

#endif // LYTE_H
